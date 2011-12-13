#!/bin/sh
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp external-editor.scm external-editor-custom.scm "$scmdir"
cp pixmaps/external-editor.png pixmaps/external-editor_dark_background.png "$pixmapsdir"
cp pixmaps/external-editor.svg pixmaps/external-editor_dark_background.svg "$pixmapsdir"
uim-module-manager --register external-editor
