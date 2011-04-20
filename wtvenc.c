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

static const ff_asf_guid wtv_guid =
    {0xB7,0xD8,0x00,0x20,0x37,0x49,0xDA,0x11,0xA6,0x4E,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid sub_wtv_guid =
    {0x8C,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid data_guid =
    {0x95,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid dir_entry_guid =
    {0x92,0xB7,0x74,0x91,0x59,0x70,0x70,0x44,0x88,0xDF,0x06,0x3B,0x82,0xCC,0x21,0x3D};

typedef struct WtvContext {
    int64_t init_root_pos;
    int64_t sector_pos;
    int64_t fat_table_pos;
    int64_t timeline_start_pos;
    int depth;
} WtvContext;

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
    CODEC_ID_PCM_S16LE,
    CODEC_ID_MPEG2VIDEO,
    wtv_write_header,
    wtv_write_packet,
    wtv_write_trailer,
};
