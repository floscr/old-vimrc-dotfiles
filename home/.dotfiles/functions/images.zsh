#!/usr/bin/env bash

# Convert video to gif file.
# Usage: video2gif video_file (scale) (fps)
function video2gif() {
  ffmpeg -y -i "${1}" -vf fps=${3:-10},scale=${2:-320}:-1:flags=lanczos,palettegen "${1}.png"
  ffmpeg -i "${1}" -i "${1}.png" -filter_complex "fps=${3:-10},scale=${2:-320}:-1:flags=lanczos[x];[x][1:v]paletteuse" "${1}".gif
  rm "${1}.png"
}

# Convert psds in a folder to jpgs and save them to the Screens Folder
function convert_psds() {
  if [[ ! -d Screens/ ]]; then
    mkdir Screens/
  fi
  for i in *.psd; do
    convert $i[0] -resize 1000 -quality 90 -filter Lanczos "Screens/${i%%.*}.jpg"
  done
}

# Convert all jpgs in current path to single pdf
function convert_to_pdf() {

  if [[ -z "$@" ]]; then
    echo "No input files given!"
    exit 1
  fi

  if [[ ! -d Screens/ ]]; then
    mkdir Screens/
  fi

  mogrify -format jpg -resize 1024 -quality 90 -filter Lanczos -path Screens "$@"

  convert Screens/*.jpg -density 280 Screens/files.pdf

}

# Resize icon for android
function andresize() {
    # Getting the res folder location from the private location.lst
    # Res folder saved und $res variable
    source ~/Dotfiles/locations.lst
    # echo "${res}"

    # xhdpi
    cp -R $1 "${res}/drawable-xhdpi/${1}"
    convert $1 -filter Lanczos -sampling-factor 1x1 -quality 90 -resize 810 "${res}/drawable-hdpi/${1}"
    convert $1 -filter Lanczos -sampling-factor 1x1 -quality 90 -resize 540 "${res}/drawable-mdpi/${1}"
    convert $1 -filter Lanczos -sampling-factor 1x1 -quality 75 -resize 405 "${res}/drawable-ldpi/${1}"
}
