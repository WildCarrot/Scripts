# Mount the SPECK drive to host the .gnupg encrypted FS.
#
# 2013-03-08
#
# Via: https://help.ubuntu.com/community/GPGKeyOnUSBDrive
#

# This assumes you store this script in the root of the
# mounted USB drive.  And since my USB drive is braindead
# and FAT formatted, I can't symlink it.
#dir=`dirname $0`
dir=/media/SPECK
loopdev=$(sudo losetup -f)

sudo -p "Password (sudo): " modprobe cryptoloop && \
sudo modprobe dm-crypt && \
sudo modprobe aes_generic && \
sudo mkdir -p /media/encrypted && \
sudo losetup $loopdev $dir/disk.img && \
sudo cryptsetup -c aes -s 256 -h sha256 create usbkey $loopdev && \
sudo mount -t ext3 /dev/mapper/usbkey /media/encrypted && \
sudo chown -R $UID.$UID /media/encrypted/
