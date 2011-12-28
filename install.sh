#!/bin/sh
srcdir=$(dirname $0)
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp "$srcdir/external-editor.scm" "$srcdir/external-editor-custom.scm" "$scmdir"
cp "$srcdir/pixmaps/external-editor.png" "$srcdir/pixmaps/external-editor_dark_background.png" "$pixmapsdir"
cp "$srcdir/pixmaps/external-editor.svg" "$srcdir/pixmaps/external-editor_dark_background.svg" "$pixmapsdir"
uim-module-manager --register external-editor
