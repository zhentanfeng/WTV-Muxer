/*
 * WTV muxer
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
#include "riff.h"
#include "asf.h"
//#include "mpegts.h"
//#include <strings.h>

#define WTV_SECTOR_BITS    12
#define WTV_BIGSECTOR_BITS 18
#define WTV_SECTOR_SIZE    (1 << WTV_SECTOR_BITS)
#define WTV_BIGSECTOR_SIZE (1 << WTV_BIGSECTOR_BITS)
#define WTV_PAD8(x) (((x) + 7) & ~7)

/* declare utf16le strings */
#define _ , 0,
static const uint8_t timeline_le16[] =
    {'t'_'i'_'m'_'e'_'l'_'i'_'n'_'e', 0};
#undef _

#define MEDIASUBTYPE_BASE_GUID \
    0x00,0x00,0x10,0x00,0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71

static const ff_asf_guid wtv_guid =
    {0xB7,0xD8,0x00,0x20,0x37,0x49,0xDA,0x11,0xA6,0x4E,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid sub_wtv_guid =
    {0x8C,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid data_guid =
    {0x95,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid dir_entry_guid =
    {0x92,0xB7,0x74,0x91,0x59,0x70,0x70,0x44,0x88,0xDF,0x06,0x3B,0x82,0xCC,0x21,0x3D};
static const ff_asf_guid stream_guid =
    {0xED,0xA4,0x13,0x23,0x2D,0xBF,0x4F,0x45,0xAD,0x8A,0xD9,0x5B,0xA7,0xF9,0x1F,0xEE};
static const ff_asf_guid format_none =
    {0xD6,0x17,0x64,0x0F,0x18,0xC3,0xD0,0x11,0xA4,0x3F,0x00,0xA0,0xC9,0x22,0x31,0x96};
static const ff_asf_guid mediatype_audio =
    {'a','u','d','s',MEDIASUBTYPE_BASE_GUID};
static const ff_asf_guid mediatype_video =
    {'v','i','d','s',MEDIASUBTYPE_BASE_GUID};

typedef struct WtvContext {
    int64_t init_root_pos;
    int64_t sector_pos;
    int64_t fat_table_pos;
    int64_t timeline_start_pos;
    int depth;
} WtvContext;

static const AVCodecGuid audio_guids[] = {
    {CODEC_ID_AC3,        {0x2C,0x80,0x6D,0xE0,0x46,0xDB,0xCF,0x11,0xB4,0xD1,0x00,0x80,0x5F,0x6C,0xBB,0xEA}},
    {CODEC_ID_EAC3,       {0xAF,0x87,0xFB,0xA7,0x02,0x2D,0xFB,0x42,0xA4,0xD4,0x05,0xCD,0x93,0x84,0x3B,0xDD}},
    {CODEC_ID_MP2,        {0x2B,0x80,0x6D,0xE0,0x46,0xDB,0xCF,0x11,0xB4,0xD1,0x00,0x80,0x5F,0x6C,0xBB,0xEA}},
    {CODEC_ID_NONE}
};

static const AVCodecGuid video_guids[] = {
    {CODEC_ID_MPEG2VIDEO, {0x26,0x80,0x6D,0xE0,0x46,0xDB,0xCF,0x11,0xB4,0xD1,0x00,0x80,0x5F,0x6C,0xBB,0xEA}},
    {CODEC_ID_NONE}
};

static int wtv_write_pad(AVIOContext *pb, int size)
{
    for(; size > 0; size--)
        avio_w8(pb, 0);
    return 0;
}

static void put_guid(AVIOContext *s, const ff_asf_guid *g)
{
    assert(sizeof(*g) == 16);
    avio_write(s, *g, sizeof(*g));
}

static int wtv_write_stream_info(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    int chunk_len = 0;
    int i = 0;

    for (; i < s->nb_streams; i++) {
        AVStream *st = s->streams[i];
        int pad;

        put_guid(pb, &stream_guid);
         // FIXME!chun_len should be caculated, for simlify we set some fixed value here.
        chunk_len = 124;
        avio_wl32(pb, chunk_len);
        avio_wl32(pb, st->index);
        wtv_write_pad(pb, 8);
        wtv_write_pad(pb, 28);

        if(st->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
            put_guid(pb, &mediatype_video);
            put_guid(pb, &video_guids[0].guid);
            wtv_write_pad(pb, 12);
            put_guid(pb,& format_none);
            avio_wl32(pb, 0);
            av_set_pts_info(st, 64, 1, st->codec->time_base.den);
        } else if (st->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
            put_guid(pb, &mediatype_audio);
            // use st->codec->codec_id to determine the GUID. we set a temp value here.
            put_guid(pb, &audio_guids[2].guid); // sub media type, the code ID should match the GUID.
            wtv_write_pad(pb, 12);
            put_guid(pb,& format_none); // set format_none
            avio_wl32(pb, 0); // since set the format_none, the size should be zero. FIXME
            av_set_pts_info(st, 64, 1, st->codec->sample_rate);
        } else {
            av_log(s, AV_LOG_ERROR, "unknown codec_type (0x%x)\n", st->codec->codec_type);
            return -1;
        }

        pad = WTV_PAD8(chunk_len) - chunk_len;
        wtv_write_pad(pb, pad);
    }
    
    return 0;
}

static int wtv_write_header(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int pad;
    put_guid(pb, &wtv_guid);
    put_guid(pb, &sub_wtv_guid);
    wtv_write_pad(pb, 16);

    //write initial root fields
    wctx->init_root_pos = avio_tell(pb);
    avio_wl32(pb, 0);  // root_size, update later
    wtv_write_pad(pb, 4);
    avio_wl32(pb, 0); // root_sector, update it later.

    pad = (1 << WTV_SECTOR_BITS) - avio_tell(pb);
    wtv_write_pad(pb, pad);
    wctx->timeline_start_pos = avio_tell(pb);

    // write stream metadata
    wtv_write_stream_info(s);
    return 0;
}

static int wtv_write_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb = s->pb;
    int chunk_len = pkt->size + 32;

     // write chunk header
    put_guid(pb, &data_guid);
    avio_wl32(pb, chunk_len);
    avio_wl32(pb, pkt->stream_index);
    wtv_write_pad(pb, 8);

    // write packet data
    avio_write(pb, pkt->data, pkt->size);

    // write padding data
    wtv_write_pad(pb, WTV_PAD8(chunk_len) - chunk_len);

    avio_flush(pb);
    return 0;
}

static int wtv_write_root_table(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int size, pad;

    put_guid(pb, &dir_entry_guid);
    avio_wl16(pb, 0); // dir_length, update later
    wtv_write_pad(pb, 6);
    avio_wl64(pb, 0); // file length, update later

    avio_wl32(pb, sizeof(timeline_le16) >> 1); // name size
    wtv_write_pad(pb, 4);
    avio_write(pb, timeline_le16, sizeof(timeline_le16)); // name

    avio_wl32(pb, wctx->fat_table_pos >> WTV_SECTOR_BITS); // first sector pointer
    avio_wl32(pb, wctx->depth); // depth

    size = avio_tell(pb) - wctx->sector_pos;
    pad = WTV_SECTOR_SIZE- size;
    wtv_write_pad(pb, pad);

    return size;
}

static int wtv_write_sector(AVFormatContext *s, int nb_sectors, int depth)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int pad;

    if(depth == 0) {
        avio_wl32(pb, wctx->timeline_start_pos >> WTV_SECTOR_BITS);
    } else if(depth == 1) {
        int i = 0;
        int64_t sector_pos = wctx->timeline_start_pos;
        int sector_pointer = sector_pos >> WTV_SECTOR_BITS;

        // write sector pointer
        for(; i < nb_sectors; i++) {
            avio_wl32(pb, sector_pointer);
            sector_pos += 1 << WTV_BIGSECTOR_BITS;
            sector_pointer = sector_pos >> WTV_BIGSECTOR_BITS;
        }

        // write left sector pointers
        wtv_write_pad(pb, (WTV_SECTOR_SIZE >> 2) - nb_sectors);
    } else if(depth == 2) {
        // size is nb_sectors1 << WTV_SECTOR_BITS
        // TODO
    } else {
        av_log(s, AV_LOG_ERROR, "unsupported file allocation table depth (0x%x)\n", wctx->depth);
        return -1;
    }

    pad = WTV_SECTOR_SIZE - (avio_tell(pb) % WTV_SECTOR_SIZE);
    wtv_write_pad(pb, pad);

    return 0;
}

static int wtv_write_trailer(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int pad;
    int depth;
    int root_size;
    uint64_t file_len;

    int64_t end_pos = avio_tell(pb);
    int timeline_file_size = (end_pos - wctx->timeline_start_pos);
    int nb_sectors = timeline_file_size >> WTV_BIGSECTOR_BITS;
    pad = WTV_BIGSECTOR_SIZE - (timeline_file_size % WTV_BIGSECTOR_SIZE);
    if (pad)
        nb_sectors++;
    wtv_write_pad(pb, pad);

    // pad to 1<< WTV_SECTOR_BITS
    pad = WTV_SECTOR_SIZE - avio_tell(pb) % WTV_SECTOR_SIZE;
    wtv_write_pad(pb, pad);

    // determine the depth of fat table
    if (nb_sectors == 1) {
        depth = 0;
    } else if (nb_sectors > (WTV_SECTOR_SIZE >> 2)) {
        depth = 2;
    } else if (nb_sectors <= (WTV_SECTOR_SIZE << 2)) {
        depth = 1;
    } else {
        av_log(s, AV_LOG_ERROR, "unsupported file allocation table depth (0x%x)\n", depth);
        return -1;
    }

    wctx->depth = depth;
    // write fat table for data
    wctx->fat_table_pos = avio_tell(pb);
    wtv_write_sector(s, nb_sectors, depth);

    // write root table
    wctx->sector_pos = avio_tell(pb);
    root_size = wtv_write_root_table(s);

    // calculate the file length
    file_len = avio_tell(pb);

    // update root value
    avio_seek(pb, wctx->init_root_pos, SEEK_SET);
    avio_wl32(pb, root_size);
    avio_seek(pb, 4, SEEK_CUR);
    avio_wl32(pb, wctx->sector_pos >> WTV_SECTOR_BITS);

    // update sector value
    avio_seek(pb, wctx->sector_pos + 16, SEEK_SET);
    avio_wl16(pb, root_size);
    avio_seek(pb, 6, SEEK_CUR);
    avio_wl64(pb, file_len);

    avio_flush(pb);
    return 0;
}


AVOutputFormat ff_wtv_muxer = {
    "wtv",
    NULL_IF_CONFIG_SMALL("Window TeleVision format"),
    NULL,
    "wtv",
    sizeof(WtvContext),
    //CODEC_ID_PCM_S16LE,
    CODEC_ID_MP2,
    CODEC_ID_MPEG2VIDEO,
    wtv_write_header,
    wtv_write_packet,
    wtv_write_trailer,
};
