#!/usr/bin/env bash

function open-bg() {
  filename=$(basename $1)
  logfile="/tmp/$USER-$filename.log"
  $1 1>$logfile 2>$logfile &
}

function compress-pdf() {

  if [ "$1" == "--help" ]; then
   echo "This command compresses a pdf to enough quality for documents with images (like passports)"
   echo "Usage:"
   echo "compress-pdf <input-file> [output-file]"
   echo "input-file: The PDF file to compress"
   echo "output-file: The name of the output file to compress, if no output-file is given"
   echo "             then a file called compressed-<input-file> is created."
  fi

  input_file=$1
  output_file=$2
  if [ "$output_file" == "" ]; then
    output_file="compressed-$(basename $input_file)"
  fi

  compression_level="/ebook"
  # /screen selects low-resolution output similar to the Acrobat Distiller "Screen Optimized" setting.
  # /ebook selects medium-resolution output similar to the Acrobat Distiller "eBook" setting.
  # /printer selects output similar to the Acrobat Distiller "Print Optimized" setting.
  # /prepress selects output similar to Acrobat Distiller "Prepress Optimized" setting.
  # /default selects output intended to be useful across a wide variety of uses, possibly at the expense of a larger output file.
  gs -dNOPAUSE -dQUIET -dBATCH \
     -sDEVICE=pdfwrite \
     -dCompatibilityLevel=1.4 \
     -dPDFSETTINGS=$compression_level \
     -sOutputFile=$output_file $input_file
  # other compression options for pdfwrite
  # -dr[resolution] -> default setting is 720dpi
  # -dDetectDuplicateImages -> defaults to true, will reuse a image used more than once
  # -dCompressPages -> defaults to true
  # -dOptimize -> defaults to false, set to true on /screen, /ebook, /printer, /prepress
  # -dCompressFonts -> defaults to true, never set to false.

}
