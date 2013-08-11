# Unmount the SPECK from encrypted FS.
#
# 2013-03-08
#
# See mount_keyring.sh for more info.
#
loopdev=$(sudo cryptsetup status usbkey | grep device | sed -e "s/ *device:[ \t]*//")

sync
sudo umount /media/encrypted
sudo cryptsetup remove usbkey
sudo losetup -d $loopdev
