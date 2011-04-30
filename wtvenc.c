/*
 * Windows Television (WTV) muxer
 * Copyright (c) 2011 Zhentan Feng <spyfeng at gmail dot com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/intreadwrite.h"
#include "avformat.h"
#include "internal.h"
#include "wtv.h"

#define WTV_BIGSECTOR_SIZE (1 << WTV_BIGSECTOR_BITS)
#define WTV_PAD8(x) (((x) + 7) & ~7)

/* declare utf16le strings */
#define _ , 0,
static const uint8_t timeline_le16[] =
    {'t'_'i'_'m'_'e'_'l'_'i'_'n'_'e', 0};
#undef _

static const ff_asf_guid sub_wtv_guid =
    {0x8C,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};

typedef struct {
    int64_t init_root_pos;
    int64_t timeline_start_pos;
} WtvContext;

static int write_pad(AVIOContext *pb, int size)
{
    for (; size > 0; size--)
        avio_w8(pb, 0);
    return 0;
}

static void put_guid(AVIOContext *s, const ff_asf_guid *g)
{
    assert(sizeof(*g) == 16);
    avio_write(s, *g, sizeof(*g));
 }

static const ff_asf_guid *get_codec_guid(enum CodecID id, const AVCodecGuid *av_guid)
{
    int i;
    for (i = 0; av_guid[i].id != CODEC_ID_NONE; i++) {
        if (id == av_guid[i].id)
            return &(av_guid[i].guid);
    }
    return NULL;
}

static int write_stream_info(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    int i = 0;

    for (; i < s->nb_streams; i++) {
        int chunk_len = 0;
        AVStream *st = s->streams[i];

        put_guid(pb, &ff_stream_guid);
        avio_wl32(pb, 0);
        avio_wl32(pb, st->index);
        write_pad(pb, 8);
        write_pad(pb, 28);
        chunk_len = 32;

        if (st->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
            const ff_asf_guid *g = get_codec_guid(st->codec->codec_id, ff_video_guids);
            if (g == NULL) {
                av_log(s, AV_LOG_ERROR, "can't get video codec_id (0x%x) guid.\n", st->codec->codec_id);
                return -1;
            }
            put_guid(pb, &ff_mediatype_video);
            put_guid(pb, g);
            write_pad(pb, 12);
            put_guid(pb,&ff_format_none);
            avio_wl32(pb, 0);
            chunk_len += 92;
            av_set_pts_info(st, 64, 1, 10000000);
        } else if (st->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
            const ff_asf_guid *g = get_codec_guid(st->codec->codec_id, ff_codec_wav_guids);
            if (g == NULL) {
                av_log(s, AV_LOG_ERROR, "can't get audio codec_id (0x%x) guid.\n", st->codec->codec_id);
                return -1;
            }
            put_guid(pb, &ff_mediatype_audio);
            put_guid(pb, g); // sub media type, the code ID should match the GUID.
            write_pad(pb, 12);
            put_guid(pb,&ff_format_none); // set format_none
            avio_wl32(pb, 0); // since set the format_none, the size should be zero. FIXME
            chunk_len += 92;
            av_set_pts_info(st, 64, 1, 10000000);
        } else {
            av_log(s, AV_LOG_ERROR, "unknown codec_type (0x%x)\n", st->codec->codec_type);
            return -1;
        }

        // update the chunk_len field and pad.
        avio_seek(pb, -(chunk_len - 16), SEEK_CUR);
        avio_wl32(pb, chunk_len);
        avio_seek(pb, chunk_len - (16 + 4), SEEK_CUR);
        write_pad(pb, WTV_PAD8(chunk_len) - chunk_len);
    }

    return 0;
}

static int write_header(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int pad;
    put_guid(pb, &ff_wtv_guid);
    put_guid(pb, &sub_wtv_guid);
    write_pad(pb, 16);

    //write initial root fields
    wctx->init_root_pos = avio_tell(pb);
    avio_wl32(pb, 0);  // root_size, update later
    write_pad(pb, 4);
    avio_wl32(pb, 0); // root_sector, update it later.

    pad = (1 << WTV_SECTOR_BITS) - avio_tell(pb);
    write_pad(pb, pad);
    wctx->timeline_start_pos = avio_tell(pb);

    // write stream info
    write_stream_info(s);
    return 0;
}

static void write_timestamp(AVIOContext *pb, AVPacket *pkt)
{
    put_guid(pb, &ff_timestamp_guid);
    avio_wl32(pb, 32 + 16);
    avio_wl32(pb, pkt->stream_index);
    write_pad(pb, 8);
    write_pad(pb, 8);
    avio_wl64(pb, pkt->pts = AV_NOPTS_VALUE ? -1 : pkt->pts);
}

static int write_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb = s->pb;
    int chunk_len = pkt->size + 32;

    // write timestamp chunk
    write_timestamp(pb, pkt);

     // write chunk header
    put_guid(pb, &ff_data_guid);
    avio_wl32(pb, chunk_len);
    avio_wl32(pb, pkt->stream_index);
    write_pad(pb, 8);

    // write packet data
    avio_write(pb, pkt->data, pkt->size);

    // write padding data
    write_pad(pb, WTV_PAD8(chunk_len) - chunk_len);

    avio_flush(pb);
    return 0;
}

static int write_root_table(AVFormatContext *s, uint64_t file_length, int sector_bits, int depth, int64_t sector_pos, int64_t fat_table_pos)
{
    AVIOContext *pb = s->pb;
    int size, pad;

    put_guid(pb, &ff_dir_entry_guid);
    avio_wl16(pb, 48 + sizeof(timeline_le16)); // dir_length
    write_pad(pb, 6);
    if (sector_bits == WTV_SECTOR_BITS)
        file_length |= 1ULL<<63;
    avio_wl64(pb, file_length); // file length

    avio_wl32(pb, sizeof(timeline_le16) >> 1); // name size
    write_pad(pb, 4);
    avio_write(pb, timeline_le16, sizeof(timeline_le16)); // name

    avio_wl32(pb, fat_table_pos >> WTV_SECTOR_BITS); // first sector pointer
    avio_wl32(pb, depth);

    size = avio_tell(pb) - sector_pos;
    pad = WTV_SECTOR_SIZE- size;
    write_pad(pb, pad);

    return size;
}

static void write_fat(AVIOContext *pb, int start_sector, int nb_sectors, int shift)
{
    int i;
    for (i = 0; i < nb_sectors; i++) {
        avio_wl32(pb, start_sector + (i << shift));
    }
    // pad left sector pointer size
    write_pad(pb, WTV_SECTOR_SIZE - (nb_sectors << 2));
}

static int write_fat_sector(AVFormatContext *s, int nb_sectors, int sector_bits, int depth)
{
    WtvContext *wctx = s->priv_data;

    int64_t start_sector = wctx->timeline_start_pos >> WTV_SECTOR_BITS;
    int shift = sector_bits - WTV_SECTOR_BITS;

    int64_t fat = avio_tell(s->pb);
    write_fat(s->pb, start_sector, nb_sectors, shift);

    if (depth == 2) {
        int64_t start_sector1 = fat >> WTV_SECTOR_BITS;
        int nb_sectors1 = ((nb_sectors << 2) + WTV_SECTOR_SIZE - 1) / WTV_SECTOR_SIZE;
        int64_t fat1 = avio_tell(s->pb);

       write_fat(s->pb, start_sector1, nb_sectors1, 0);
       return fat1;
    }

    return fat;
}

static int write_trailer(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int pad;
    int depth;
    int sector_bits;
    int root_size;
    int nb_sectors;
    int64_t sector_pos, fat_table_pos;

    int64_t end_pos = avio_tell(pb);
    int64_t timeline_file_size = (end_pos - wctx->timeline_start_pos);

    // determine optimal fat table depth, sector_bits, nb_sectors
    if (timeline_file_size <= WTV_SECTOR_SIZE) {
        depth = 0;
        sector_bits = WTV_SECTOR_BITS;
    } else if (timeline_file_size <= (WTV_SECTOR_SIZE / 4) * WTV_SECTOR_SIZE) {
        depth = 1;
        sector_bits = WTV_SECTOR_BITS;
    } else if (timeline_file_size <= (WTV_SECTOR_SIZE / 4) * WTV_BIGSECTOR_SIZE) {
        depth = 1;
        sector_bits = WTV_BIGSECTOR_BITS;
    } else if (timeline_file_size <= (int64_t)(WTV_SECTOR_SIZE / 4) * (WTV_SECTOR_SIZE / 4) * WTV_SECTOR_SIZE) {
        depth = 2;
        sector_bits = WTV_SECTOR_BITS;
    } else if (timeline_file_size <= (int64_t)(WTV_SECTOR_SIZE / 4) * (WTV_SECTOR_SIZE / 4) * WTV_BIGSECTOR_SIZE) {
        depth = 2;
        sector_bits = WTV_BIGSECTOR_BITS;
    } else {
        av_log(s, AV_LOG_ERROR, "unsupported file allocation table depth (%"PRIi64" bytes)\n", timeline_file_size);
        return -1;
    }

    // determine the nb_sectors
    nb_sectors = (int)(timeline_file_size >> sector_bits);

    // pad sector of timeline
    pad = (1 << sector_bits) - (timeline_file_size % (1 << sector_bits));
    if (pad) {
        nb_sectors++;
        write_pad(pb, pad);
    }

    //write fat table
    if (depth > 0) {
        fat_table_pos = write_fat_sector(s, nb_sectors, sector_bits, depth);
    } else {
        fat_table_pos = wctx->timeline_start_pos >> WTV_SECTOR_BITS;
    }

    // write root table
    sector_pos = avio_tell(pb);
    root_size = write_root_table(s, timeline_file_size, sector_bits, depth, sector_pos, fat_table_pos);

    // update root value
    avio_seek(pb, wctx->init_root_pos, SEEK_SET);
    avio_wl32(pb, root_size);
    avio_seek(pb, 4, SEEK_CUR);
    avio_wl32(pb, sector_pos >> WTV_SECTOR_BITS);

    avio_flush(pb);
    return 0;
}


AVOutputFormat ff_wtv_muxer = {
    "wtv",
    NULL_IF_CONFIG_SMALL("Windows Television (WTV)"),
    NULL,
    "wtv",
    sizeof(WtvContext),
    CODEC_ID_MP2,
    CODEC_ID_MPEG2VIDEO,
    write_header,
    write_packet,
    write_trailer,
    .codec_tag= (const AVCodecTag* const []){ff_codec_bmp_tags, ff_codec_wav_tags, 0},
};
