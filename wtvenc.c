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

// ---
// FIXME: when stable, move this stuff to wtv.h

#define WTV_BIGSECTOR_SIZE (1 << WTV_BIGSECTOR_BITS)
#define WTV_PAD8(x) (((x) + 7) & ~7)
#define INDEX_BASE 0x2
#define MAX_NB_INDEX 10

/* declare utf16le strings */
#define _ , 0,
static const uint8_t timeline_table_0_header_events[] =
    {'t'_'i'_'m'_'e'_'l'_'i'_'n'_'e'_'.'_'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'h'_'e'_'a'_'d'_'e'_'r'_'.'_'E'_'v'_'e'_'n'_'t'_'s', 0};
static const uint8_t timeline_table_0_entries_Events_le16[] =
    {'t'_'i'_'m'_'e'_'l'_'i'_'n'_'e'_'.'_'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'e'_'n'_'t'_'r'_'i'_'e'_'s'_'.'_'E'_'v'_'e'_'n'_'t'_'s', 0};
static const uint8_t timeline_le16[] =
    {'t'_'i'_'m'_'e'_'l'_'i'_'n'_'e', 0};
static const uint8_t table_0_header_legacy_attrib[] =
    {'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'h'_'e'_'a'_'d'_'e'_'r'_'.'_'l'_'e'_'g'_'a'_'c'_'y'_'_'_'a'_'t'_'t'_'r'_'i'_'b', 0};
static const uint8_t table_0_entries_legacy_attrib[] =
    {'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'e'_'n'_'t'_'r'_'i'_'e'_'s'_'.'_'l'_'e'_'g'_'a'_'c'_'y'_'_'_'a'_'t'_'t'_'r'_'i'_'b', 0};
static const uint8_t table_0_redirector_legacy_attrib[] =
    {'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'r'_'e'_'d'_'i'_'r'_'e'_'c'_'t'_'o'_'r'_'.'_'l'_'e'_'g'_'a'_'c'_'y'_'_'_'a'_'t'_'t'_'r'_'i'_'b', 0};
static const uint8_t table_0_header_time[] =
    {'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'h'_'e'_'a'_'d'_'e'_'r'_'.'_'t'_'i'_'m'_'e', 0};
static const uint8_t table_0_entries_time[] =
    {'t'_'a'_'b'_'l'_'e'_'.'_'0'_'.'_'e'_'n'_'t'_'r'_'i'_'e'_'s'_'.'_'t'_'i'_'m'_'e', 0};
static const uint8_t legacy_attrib[] =
    {'l'_'e'_'g'_'a'_'c'_'y'_'_'_'a'_'t'_'t'_'r'_'i'_'b', 0};
#undef _

static const ff_asf_guid DSATTRIB_TRANSPORT_PROPERTIES =
    {0x12,0xF6,0x22,0xB6,0xAD,0x47,0x71,0x46,0xAD,0x6C,0x05,0xA9,0x8E,0x65,0xDE,0x3A};

static const ff_asf_guid sub_wtv_guid =
    {0x8C,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid metadata_guid =
    {0x5A,0xFE,0xD7,0x6D,0xC8,0x1D,0x8F,0x4A,0x99,0x22,0xFA,0xB1,0x1C,0x38,0x14,0x53};
static const ff_asf_guid stream1_guid =
    {0xA1,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid stream2_guid =
    {0xA2,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid sync_guid =
    {0x97,0xC3,0xD2,0xC2,0x7E,0x9A,0xDA,0x11,0x8B,0xF7,0x00,0x07,0xE9,0x5E,0xAD,0x8D};
static const ff_asf_guid index_guid =
    {0x96,0xc3,0xd2,0xc2,0x7e,0x9a,0xda,0x11,0x8b,0xf7,0x00,0x07,0xe9,0x5e,0xad,0x8d};

/* Media subtypes */
static const ff_asf_guid mediasubtype_cpfilters_processed =
    {0x28,0xBD,0xAD,0x46,0xD0,0x6F,0x96,0x47,0x93,0xB2,0x15,0x5C,0x51,0xDC,0x04,0x8D};

/* Formats */
static const ff_asf_guid format_cpfilters_processed =
    {0x6F,0xB3,0x39,0x67,0x5F,0x1D,0xC2,0x4A,0x81,0x92,0x28,0xBB,0x0E,0x73,0xD1,0x6A};
static const ff_asf_guid format_waveformatex =
    {0x81,0x9F,0x58,0x05,0x56,0xC3,0xCE,0x11,0xBF,0x01,0x00,0xAA,0x00,0x55,0x59,0x5A};
static const ff_asf_guid format_mpeg2_video =
    {0xE3,0x80,0x6D,0xE0,0x46,0xDB,0xCF,0x11,0xB4,0xD1,0x00,0x80,0x5F,0x6C,0xBB,0xEA};

static const ff_asf_guid mp2_guid =
    {0x2B,0x80,0x6D,0xE0,0x46,0xDB,0xCF,0x11,0xB4,0xD1,0x00,0x80,0x5F,0x6C,0xBB,0xEA}; //FIXME: copied from riff.c struct

static const ff_asf_guid codec_id_mpeg2video =
    {0x26,0x80,0x6D,0xE0,0x46,0xDB,0xCF,0x11,0xB4,0xD1,0x00,0x80,0x5F,0x6C,0xBB,0xEA}; //FIXME: copied from wtv.c struct

enum WtvFileIndex {
    WTV_TIMELINE_TABLE_0_HEADER_EVENTS = 0,
    WTV_TIMELINE_TABLE_0_ENTRIES_EVENTS,
    WTV_TIMELINE,
    WTV_TABLE_0_HEADER_LEGACY_ATTRIB,
    WTV_TABLE_0_ENTRIES_LEGACY_ATTRIB,
    WTV_TABLE_0_REDIRECTOR_LEGACY_ATTRIB,
    WTV_TABLE_0_HEADER_TIME,
    WTV_TABLE_0_ENTRIES_TIME,
    WTV_FILES
};

static const uint8_t * ff_wtv_filename[] = {
    timeline_table_0_header_events,
    timeline_table_0_entries_Events_le16,
    timeline_le16,
    table_0_header_legacy_attrib,
    table_0_entries_legacy_attrib,
    table_0_redirector_legacy_attrib,
    table_0_header_time,
    table_0_entries_time
};

static const uint8_t ff_wtv_filename_len[] = {
    sizeof(timeline_table_0_header_events),
    sizeof(timeline_table_0_entries_Events_le16),
    sizeof(timeline_le16),
    sizeof(table_0_header_legacy_attrib),
    sizeof(table_0_entries_legacy_attrib),
    sizeof(table_0_redirector_legacy_attrib),
    sizeof(table_0_header_time),
    sizeof(table_0_entries_time),
};

typedef struct {
    int64_t length;
    const void *header;
    int depth;
    int first_sector;
} WtvFile;

typedef struct {
    int64_t             pos;
    int64_t             serial;
    const ff_asf_guid * guid;
    int                 stream_id;
} WtvIndexEntry;

typedef struct {
    int64_t timeline_start_pos;
    WtvFile file[WTV_FILES];
    int64_t serial;          //chunk serial number
    int64_t last_chunk_pos;  // last chunk position

    //FIXME: rename to 'header' entries or sth more meaningful
    WtvIndexEntry index[MAX_NB_INDEX];
    int nb_index;

 //FIXME: the 'index_guid' chunk makes reference to previous index chunks, but this feature seems optional
//     implemented for testing, remove it later
#define MAX_NB_CHUNKS  100
    int64_t chunks[MAX_NB_CHUNKS];
    int nb_chunks;
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

/**
 * Write chunk header. If header chunk (0x80000000 set) then add to list of header chunks
 */
static void write_chunk_header(AVFormatContext *s, const ff_asf_guid *guid, int length, int stream_id)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;

    wctx->last_chunk_pos = avio_tell(pb) - wctx->timeline_start_pos;
    put_guid(pb, guid);
    avio_wl32(pb, 32 + length);
    avio_wl32(pb, stream_id);
    avio_wl64(pb, wctx->serial);

    if ((stream_id & 0x80000000) && guid != &index_guid) {
        WtvIndexEntry *t = wctx->index + wctx->nb_index;
        t->pos       = wctx->last_chunk_pos;
        t->serial    = wctx->serial;
        t->guid      = guid;
        t->stream_id = stream_id & 0x3FFFFFFF;
        wctx->nb_index++;
    }
}

static void write_chunk_header2(AVFormatContext *s, const ff_asf_guid *guid, int length, int stream_id)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;

    int64_t last_chunk_pos = wctx->last_chunk_pos;
    write_chunk_header(s, guid, length + 8, stream_id);
    avio_wl64(pb, last_chunk_pos);
}

static void finish_chunk_noindex(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;

    // update the chunk_len field and pad.
    int64_t chunk_len = avio_tell(pb) - (wctx->last_chunk_pos + wctx->timeline_start_pos);
    avio_seek(pb, -(chunk_len - 16), SEEK_CUR);
    avio_wl32(pb, chunk_len);
    avio_seek(pb, chunk_len - (16 + 4), SEEK_CUR);

    write_pad(pb, WTV_PAD8(chunk_len) - chunk_len);
    wctx->serial++;
}

static void write_index(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    WtvContext *wctx = s->priv_data;
    int chunk_len = 8 + wctx->nb_chunks * 8 + wctx->nb_index * 0x28;
    int i;

    write_chunk_header2(s, &index_guid, chunk_len, 0x80000000);
#if 1
    /* FIXME: optional feature */
    avio_wl32(pb, wctx->nb_chunks);
    avio_wl32(pb, 0); // checksum?
    for (i = 0; i < wctx->nb_chunks; i++)
        avio_wl64(pb, wctx->chunks[i]);
    wctx->chunks[wctx->nb_chunks] = wctx->last_chunk_pos;
    wctx->nb_chunks++;
#else
    avio_wl32(pb, 0);
    avio_wl32(pb, 0x649f70);
#endif

    for (i = 0; i < wctx->nb_index; i++) {
        WtvIndexEntry *t = wctx->index + i;
        put_guid(pb,  t->guid);
        avio_wl64(pb, t->pos);
        avio_wl32(pb, t->stream_id);
        avio_wl32(pb, 0); // checksum?
        avio_wl64(pb, t->serial);
    }
    wctx->nb_index = 0; // reset index
    finish_chunk_noindex(s);
}

static void finish_chunk(AVFormatContext *s, int length)
{
    WtvContext *wctx = s->priv_data;
    finish_chunk_noindex(s);
    if (wctx->nb_index == MAX_NB_INDEX)
        write_index(s);
}

static void write_stream1_vid(AVFormatContext *s, int stream_id)
{
    AVIOContext *pb = s->pb;
    const uint8_t format[] = {
 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x02, 0x00, 0x00,
 0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 0x00, 0x00, 0x00, 0x00, 0x00, 0x1b, 0xb7, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb1, 0x8b, 0x02, 0x00,
 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00,
 0xc0, 0x02, 0x00, 0x00, 0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4d, 0x50, 0x45, 0x47,
 0x00, 0x00, 0x00, 0x00, 0xd0, 0x07, 0x00, 0x00, 0x42, 0xd8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 0x00, 0x00, 0x00, 0x00, 0xc0, 0x27, 0xc8, 0x00, 0x4c, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xb3, 0x2c, 0x01, 0xe0, 0x37,
 0x1d, 0x4c, 0x23, 0x81, 0x10, 0x11, 0x11, 0x12, 0x12, 0x12, 0x13, 0x13, 0x13, 0x13, 0x14, 0x14,
 0x14, 0x14, 0x14, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16, 0x16,
 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x17, 0x18, 0x18, 0x18, 0x19, 0x18, 0x18, 0x18, 0x19,
 0x1a, 0x1a, 0x1a, 0x1a, 0x19, 0x1b, 0x1b, 0x1b, 0x1b, 0x1b, 0x1c, 0x1c, 0x1c, 0x1c, 0x1e, 0x1e,
 0x1e, 0x1f, 0x1f, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    write_chunk_header2(s, &stream1_guid, 0x144, 0x80000000 | stream_id);

    avio_wl32(pb, stream_id);
    write_pad(pb, 4);

    write_pad(pb, 4);
    put_guid(pb, &ff_mediatype_video);
    put_guid(pb, &mediasubtype_cpfilters_processed);
    avio_wl32(pb, 1); //FIXME: unknown
    avio_wl32(pb, 1); //FIXME: unknown
    avio_wl16(pb, 0); //FIXME: unknown
    avio_wl16(pb, 1); //FIXME: unknown
    put_guid(pb, &format_cpfilters_processed);

    avio_wl32(pb, sizeof(format) + 32);
    avio_write(pb, format, sizeof(format));
    put_guid(pb, &codec_id_mpeg2video);
    put_guid(pb, &format_mpeg2_video);

    finish_chunk(s, 0x144);
}

static void write_stream1(AVFormatContext *s, int stream_id)
{
    AVIOContext *pb = s->pb;
    static const uint8_t x[] = {
        0x50, 0x00, 0x02, 0x00, 0x80, 0xbb, 0x00, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00,
        0x16, 0x00, 0x02, 0x00, 0x00, 0xe8, 0x03, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x1c, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    write_chunk_header2(s, &stream1_guid, 0x94, 0x80000000 | stream_id);

    avio_wl32(pb, stream_id);
    write_pad(pb, 4);

    write_pad(pb, 4);
    put_guid(pb, &ff_mediatype_audio);
    put_guid(pb, &mediasubtype_cpfilters_processed);
    avio_wl32(pb, 0); //FIXME: unknown
    avio_wl32(pb, 0); //FIXME: unknown
    avio_wl16(pb, 0); //FIXME: unknown
    avio_wl16(pb, 1); //FIXME: unknown
    put_guid(pb, &format_cpfilters_processed);

    avio_wl32(pb, 0x28 + 32); // size of data, plus two guids

    avio_write(pb, x, sizeof(x));
    put_guid(pb, &mp2_guid);
    put_guid(pb, &format_waveformatex);

    finish_chunk(s, 0x94);
}

static void write_sync(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    WtvContext *wctx = s->priv_data;
    int64_t last_chunk_pos = wctx->last_chunk_pos;

    write_chunk_header(s, &sync_guid, 0x18, 0);
    avio_wl64(pb, 0);  // FIXME: file-offset to previous keyframe
    avio_wl64(pb, -1); // FIXME: ??
    avio_wl64(pb, 0);  // FIXME: ??

    finish_chunk(s, 0x18);

    wctx->last_chunk_pos = last_chunk_pos;
}

static void write_DSATTRIB_TRANSPORT_PROPERTIES_init(AVFormatContext *s, int stream_index)
{
    AVIOContext *pb = s->pb;
    write_chunk_header2(s, &DSATTRIB_TRANSPORT_PROPERTIES, 0x18, 0x80000000 | stream_index);
    avio_wl64(pb, 0xc9); // identifier (used by subsequence PROPERTIES)
    avio_wl64(pb, -1);
    avio_wl64(pb, 0);
    finish_chunk(s, 0x18);
}

static int write_stream_data(AVFormatContext *s, AVStream *st, int flag)
{
    AVIOContext *pb = s->pb;
    const ff_asf_guid *g, *media_type, *format_type;
    int64_t  hdr_pos_start, start, chunk_len;
    int hdr_size = 0;
    
    start = avio_tell(pb);

    if (!flag) {
        write_chunk_header2(s, &ff_stream_guid, 0x00/*FIXME*/, 0x80000000 | (st->index + INDEX_BASE));
        avio_wl32(pb, 0x00000001); // pad
        avio_wl32(pb, st->index + INDEX_BASE); //stream_id
        avio_wl32(pb, 0x00000001); // pad
        write_pad(pb, 8); //pad
    } else {
        write_chunk_header2(s, &stream2_guid, 0x00/*FIXME*/, 0x80000000 | (st->index + INDEX_BASE));
        write_pad(pb, 4); // pad
    }

    if (st->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
        g = get_codec_guid(st->codec->codec_id, ff_video_guids);
        media_type = &ff_mediatype_video;
        format_type = &format_mpeg2_video;
    } else if (st->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
        g = get_codec_guid(st->codec->codec_id, ff_codec_wav_guids);
        media_type = &ff_mediatype_audio;
        format_type = &format_waveformatex;
    }  else {
        av_log(s, AV_LOG_ERROR, "unknown codec_type (0x%x)\n", st->codec->codec_type);
        return -1;
    }

    if (g == NULL) {
        av_log(s, AV_LOG_ERROR, "can't get video codec_id (0x%x) guid.\n", st->codec->codec_id);
        return -1;
    }

    put_guid(pb, media_type); // mediatype
    put_guid(pb, &mediasubtype_cpfilters_processed); // subtype
    write_pad(pb, 12);
    put_guid(pb,&format_cpfilters_processed); // format type
    avio_wl32(pb, 0); // size
    
    hdr_pos_start = avio_tell(pb);
    if (st->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
        write_pad(pb, 72); // aspect ratio
        ff_put_bmp_header(pb, st->codec, ff_codec_bmp_tags, 0);
    } else {
        ff_put_wav_header(pb, st->codec);
    }
    hdr_size = avio_tell(pb) - hdr_pos_start;
    
    // seek back write hdr_size
    avio_seek(pb, -(hdr_size + 4), SEEK_CUR);
    avio_wl32(pb, hdr_size + 32);
    avio_seek(pb, hdr_size, SEEK_CUR);
    put_guid(pb, g); // actual_subtype
    put_guid(pb, format_type); // actual_formattype

    finish_chunk(s, chunk_len);

    av_set_pts_info(st, 64, 1, 10000000);

    return 0;
}

static int write_stream_info_new(AVFormatContext *s)
{
    int i = 0;
    for (; i < s->nb_streams; i++) {
        int ret;
        AVStream *st = s->streams[i];
        ret  = write_stream_data(s, st, 0);
        if (ret < 0) {
            av_log(s, AV_LOG_ERROR, "write stream data failed codec_type(0x%x)\n", st->codec->codec_type);
            return -1;
        }
        ret = write_stream_data(s, st, 1);
        if (ret < 0) {
            av_log(s, AV_LOG_ERROR, "write stream2 data failed codec_type(0x%x)\n", st->codec->codec_type);
            return -1;
        }
    }
    return 0;
}

static int write_header(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    WtvContext *wctx = s->priv_data;
    int i, pad;
    put_guid(pb, &ff_wtv_guid);
    put_guid(pb, &sub_wtv_guid);

    // It seems fixed value. unknown meanings.
    avio_wl32(pb, 0x01);
    avio_wl32(pb, 0x02);
    avio_wl32(pb, 0x1000);
    avio_wl32(pb, 0x040000);

    //write initial root fields
    avio_wl32(pb, 0);  // root_size, update later
    write_pad(pb, 4);
    avio_wl32(pb, 0); // root_sector, update it later.

    write_pad(pb, 32);
    avio_wl32(pb, 0x0600); // unknown field. should not be 0x0000, 0x0*00 seems ok

    pad = (1 << WTV_SECTOR_BITS) - avio_tell(pb);
    write_pad(pb, pad);
    wctx->timeline_start_pos = avio_tell(pb);

    wctx->serial = 1;
    wctx->last_chunk_pos = -1;

    // the very first chunk must be 'stream1_guid'
    write_stream1_vid(s, 0x1);  //a video stream must be present for WMC to decode, even if there is no video stream
                                //DirectShow 'StreamBufferSource' will work without this
    write_sync(s);

    write_stream1(s, 0x1);

    //write_stream_info(s);  // must be placed after stream1, and before DSATTRIB_TRANSPORT_PROPERTIES
    write_stream_info_new(s);
    for (i = 0; i < s->nb_streams; i++)
        write_DSATTRIB_TRANSPORT_PROPERTIES_init(s, INDEX_BASE + i);

    if (wctx->nb_index)
        write_index(s);

    return 0;
}

static void write_timestamp(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb = s->pb;
    write_chunk_header(s, &ff_timestamp_guid, 56, 0x40000000 | (INDEX_BASE + pkt->stream_index));
    write_pad(pb, 8);
    avio_wl64(pb, pkt->pts == AV_NOPTS_VALUE ? -1 : pkt->pts);
    avio_wl64(pb, pkt->pts == AV_NOPTS_VALUE ? -1 : pkt->pts);
    avio_wl64(pb, 0x57e400);  //FIXME: required for WMC playback; most likely the 'duration' of the data packet (time unit unknown)
    avio_wl64(pb, 0);
    avio_wl64(pb, 1);
    avio_wl64(pb, 0);
}

static int write_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb = s->pb;
    WtvContext  *wctx = s->priv_data;

    // write timestamp chunk
    write_timestamp(s, pkt);

    write_chunk_header(s, &ff_data_guid, pkt->size, INDEX_BASE + pkt->stream_index);
    avio_write(pb, pkt->data, pkt->size);
    write_pad(pb, WTV_PAD8(pkt->size) - pkt->size);

    wctx->serial++;
    avio_flush(pb);
    return 0;
}

static void write_dir_entry(AVFormatContext *s, enum WtvFileIndex index)
{
    AVIOContext *pb   = s->pb;
    WtvContext  *wctx = s->priv_data;
    WtvFile     *w    = &wctx->file[index];
    int filename_padding = WTV_PAD8(ff_wtv_filename_len[index]) - ff_wtv_filename_len[index];

    put_guid(pb, &ff_dir_entry_guid);
    avio_wl16(pb, 40 + ff_wtv_filename_len[index] + filename_padding + (w->header ? w->length : 8));
    write_pad(pb, 6);
    avio_wl64(pb, w->length);
    avio_wl32(pb, (ff_wtv_filename_len[index] + filename_padding) >> 1);
    write_pad(pb, 4);

    avio_write(pb, ff_wtv_filename[index], ff_wtv_filename_len[index]);
    write_pad(pb, filename_padding);

    if (w->header) {
        avio_write(pb, w->header, w->length);
    } else {
        avio_wl32(pb, w->first_sector);
        avio_wl32(pb, w->depth);
    }
}

static int write_root_table(AVFormatContext *s, int64_t sector_pos)
{
    AVIOContext *pb = s->pb;
    int size, pad;
    int i;

    for (i = 0; i < WTV_FILES; i++)
        write_dir_entry(s, i);

    // caculate root table size
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

static int write_fat_sector(AVFormatContext *s, int64_t start_pos, int nb_sectors, int sector_bits, int depth)
{
    int64_t start_sector = start_pos >> WTV_SECTOR_BITS;
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

// table 2
static void write_table_entries_events(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    //WtvContext *wctx = s->priv_data;

    //FIXME: output frame_nb, position pairs
    //avio_wl64(pb, 0x2);   avio_wl64(pb, 0xc0);
    avio_wl64(pb, 0x2);   avio_wl64(pb, 0x170);
    //avio_wl64(pb, 0x2);   avio_wl64(pb, 0x188);
}

static void write_tag(AVIOContext *pb, const char *key, const char *value)
{
    put_guid(pb, &metadata_guid);
    avio_wl32(pb, 1);
    avio_wl32(pb, strlen(value)*2 + 2);
    avio_put_str16le(pb, key);
    avio_put_str16le(pb, value);
}

static void write_table_entries_attrib(AVFormatContext *s)
{
    AVDictionaryEntry *tag = 0;

    //FIXME: translate special tags (e.g. WM/Bitrate) to binary representation
    ff_metadata_conv(&s->metadata, ff_asf_metadata_conv, NULL);
    while((tag = av_dict_get(s->metadata, "", tag, AV_DICT_IGNORE_SUFFIX)))
        write_tag(s->pb, tag->key, tag->value);
}

static void write_table_redirector_legacy_attrib(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    AVDictionaryEntry *tag = 0;
    int64_t pos = 0;

    //FIXME: translate special tags to binary representation
    while((tag = av_dict_get(s->metadata, "", tag, AV_DICT_IGNORE_SUFFIX))) {
        avio_wl64(pb, pos);
        pos += 16 + 4 + 4 + strlen(tag->key)*2 + 2 + strlen(tag->value)*2 + 2;
    }
}

// table 7
// The content can be pad as zeroes because it is not necessary for playback.
static void write_table_entries_time(AVFormatContext *s)
{
    //AVIOContext *pb = s->pb;
    //WtvContext *wctx = s->priv_data;

#if 0
    //avio_wl64(pb, 0x0);      avio_wl64(pb, 0x30);
    //avio_wl64(pb, 0x4c4b40); avio_wl64(pb, 0x43);
    //avio_wl64(pb, 0x7c8300); avio_wl64(pb, 0x50);
#else
    //FIXME: output timestamp, frame_nb pairs
#endif
}

/**
 * Pad the remainder of a file
 * Write out fat table
 * @return <0 on error
 */
static int finish_file(AVFormatContext *s, enum WtvFileIndex index, int64_t start_pos)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    WtvFile *w = &wctx->file[index];
    int64_t end_pos = avio_tell(pb);
    int sector_bits, nb_sectors, pad;

    w->length = (end_pos - start_pos);

    // determine optimal fat table depth, sector_bits, nb_sectors
    if (w->length <= WTV_SECTOR_SIZE) {
        w->depth = 0;
        sector_bits = WTV_SECTOR_BITS;
    } else if (w->length <= (WTV_SECTOR_SIZE / 4) * WTV_SECTOR_SIZE) {
        w->depth = 1;
        sector_bits = WTV_SECTOR_BITS;
    } else if (w->length <= (WTV_SECTOR_SIZE / 4) * WTV_BIGSECTOR_SIZE) {
        w->depth = 1;
        sector_bits = WTV_BIGSECTOR_BITS;
    } else if (w->length <= (int64_t)(WTV_SECTOR_SIZE / 4) * (WTV_SECTOR_SIZE / 4) * WTV_SECTOR_SIZE) {
        w->depth = 2;
        sector_bits = WTV_SECTOR_BITS;
    } else if (w->length <= (int64_t)(WTV_SECTOR_SIZE / 4) * (WTV_SECTOR_SIZE / 4) * WTV_BIGSECTOR_SIZE) {
        w->depth = 2;
        sector_bits = WTV_BIGSECTOR_BITS;
    } else {
        av_log(s, AV_LOG_ERROR, "unsupported file allocation table depth (%"PRIi64" bytes)\n", w->length);
        return -1;
    }

    // determine the nb_sectors
    nb_sectors = (int)(w->length >> sector_bits);

    // pad sector of timeline
    pad = (1 << sector_bits) - (w->length % (1 << sector_bits));
    if (pad) {
        nb_sectors++;
        write_pad(pb, pad);
    }

    //write fat table
    if (w->depth > 0) {
        w->first_sector = write_fat_sector(s, start_pos, nb_sectors, sector_bits, w->depth);
    } else {
        w->first_sector = start_pos;
    }
    w->first_sector >>= WTV_SECTOR_BITS;

    w->length |= 1ULL<<60;
    if (sector_bits == WTV_SECTOR_BITS)
        w->length |= 1ULL<<63;

    return 0;
}

static void finish_header(AVFormatContext *s, enum WtvFileIndex index, const void *data, int64_t length)
{
    WtvContext *wctx = s->priv_data;
    WtvFile *w = &wctx->file[index];

    w->length = length | (1ULL<<62) | (1ULL<<60);
    w->header = data;
}

static const uint8_t header_events[] = {
0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0x45, 0x00, 0x76, 0x00, 0x65, 0x00, 0x6e, 0x00, 0x74, 0x00, 0x73, 0x00, 0x00, 0x00, // 'Events'
0x8d, 0x04, 0x00, 0x00, 0x00, 0x00,
0x68, 0xa2, 0x49, 0x05,
0xf8, 0xf3, 0x86, 0x01, 0x26, 0x7a, 0xdb, 0x77, 0x18, 0x71, 0xe3, 0x77, 0xfc, 0x9d, 0x86, 0x04,
0xa8, 0xae, 0x8d, 0x04, 0x00, 0x00, 0x00, 0x00, 0xe4, 0xf3, 0x86, 0x01, 0xc0, 0x3c, 0x9a, 0x6b,
0xdc, 0x76, 0x0b, 0x01, 0xcc, 0x76, 0x0b, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x32, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

static const uint8_t header_legacy_attrib[] = {
0xff, 0xff, 0xff, 0xff, //cares
0x00, 0x00, 0x00, 0x00, //cares
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // ignores
0x6c, 0x00, 0x65, 0x00, 0x67, 0x00, 0x61, 0x00, 0x63, 0x00, 0x79, 0x00, 0x5f, 0x00, 0x61, 0x00, 0x74, 0x00, 0x74, 0x00, 0x72, 0x00, 0x69, 0x00, 0x62, 0x00, 0x00, 0x00, // 'legacy.attrib'
0x28, 0xf6, 0x08, 0x01, 0x5c, 0xf5, 0x08, 0x01, // ignores
0xca, 0x22, 0xd3, 0x63, 0x40, 0x80, 0xdf, 0x63, // ignores
0xd8, 0x65, 0x4b, 0x02, 0x60, 0xf5, 0x08, 0x01, // ignores
0xd0, 0xf8, 0x11, 0x77, 0xec, 0x65, 0x4b, 0x02, // ignores
0x00, 0x00, 0x00, 0x00 //ignores
};

static const uint8_t header_time[] = {
0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0x74, 0x00, 0x69, 0x00, 0x6d, 0x00, 0x65, 0x00, 0x00, 0x00, // 'time'

0x2e, 0x00, 0xf9, 0x18, 0xd7, 0x63, 0x5c, 0xde, 0x2e, 0x00, 0x8b, 0x84, 0x50, 0x35, 0xd0, 0xf4,
0x08, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0xd8, 0xf4, 0x08, 0x01, 0x45, 0x9d, 0xa2, 0x75, 0x00, 0x00, 0x2e, 0x00, 0x00, 0x00,
0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x40, 0x4b, 0x4c, 0x00, 0x00, 0x00, 0x00, 0x00,
};


static int write_trailer(AVFormatContext *s)
{
    WtvContext *wctx = s->priv_data;
    AVIOContext *pb = s->pb;
    int root_size;
    int64_t sector_pos;
    int64_t start_pos;

    if (finish_file(s, WTV_TIMELINE, wctx->timeline_start_pos) < 0)
        return -1;

    finish_header(s, WTV_TIMELINE_TABLE_0_HEADER_EVENTS, header_events, sizeof(header_events));

    start_pos = avio_tell(pb);
    write_table_entries_events(s);
    if (finish_file(s, WTV_TIMELINE_TABLE_0_ENTRIES_EVENTS, start_pos) < 0)
        return -1;

    finish_header(s, WTV_TABLE_0_HEADER_LEGACY_ATTRIB, header_legacy_attrib, sizeof(header_legacy_attrib));

    start_pos = avio_tell(pb);
    write_table_entries_attrib(s);
    if (finish_file(s, WTV_TABLE_0_ENTRIES_LEGACY_ATTRIB, start_pos) < 0)
        return -1;

    start_pos = avio_tell(pb);
    write_table_redirector_legacy_attrib(s);
    if (finish_file(s, WTV_TABLE_0_REDIRECTOR_LEGACY_ATTRIB, start_pos) < 0)
        return -1;

    finish_header(s, WTV_TABLE_0_HEADER_TIME, header_time, sizeof(header_time));

    start_pos = avio_tell(pb);
    write_table_entries_time(s);
    if (finish_file(s, WTV_TABLE_0_ENTRIES_TIME, start_pos) < 0)
        return -1;

    // write root table
    sector_pos = avio_tell(pb);
    root_size = write_root_table(s, sector_pos);

    // update root value
    avio_seek(pb, 0x30, SEEK_SET);
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
