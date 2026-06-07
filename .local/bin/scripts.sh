# prepare SD card -> creates partition and formats to fat32
echo 'type=0b' | sudo sfdisk /dev/sda && sudo mkfs.fat -F 32 /dev/sda1
