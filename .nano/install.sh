wget -O /tmp/nanorc.zip https://github.com/scopatz/nanorc/archive/master.zip
if [ ! -d ~/.nano/ ]
then
    mkdir ~/.nano/
fi

cd ~/.nano/

unzip -o "/tmp/nanorc.zip"
mv nanorc-master/* ./
rm -rf nanorc-master

cat "${HOME}/.nano/nanorc" | sort -u > /tmp/nanorc
echo "Append the contents of /tmp/nanorc >> $HOME/.nanorc file"
echo "if not already there."
