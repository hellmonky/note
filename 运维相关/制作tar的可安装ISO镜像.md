# 制作tar的可安装ISO镜像

可以通过ioslinux引导，然后将当前的tar解压完成系统安装。
具体流程为：
（1）裸机上电，使用最小系统启动；
（2）清除硬盘MBR引导，创建磁盘分区；
（3）tar恢复目录结构；
（4）恢复硬盘引导记录，创建grub；

参考：
[迁移 Linux 系统，第 1 部分](https://www.ibm.com/developerworks/cn/linux/l-cn-linux-br1/index.html)
[使用tar或dd等完成Linux系统备份恢复](https://my.oschina.net/emptytimespace/blog/112508)
[使用ISOLinux制作Linux系统安装盘](http://blog.csdn.net/liujixin8/article/details/4029887)
[基于tar包制作启动光盘的步骤](http://blog.csdn.net/wangyezi19930928/article/details/52955973)

阿里云ATA上有具体的方式：
[怎样制作Centos7 U盘安装镜像](https://www.atatech.org/articles/86748)
参考了：
[WORKING WITH ISO IMAGES](https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/anaconda_customization_guide/sect-iso-images)





首先，挂载可以启动的centos基础镜像，然后提取内容到缓存中：
mkdir /mnt/iso
mount -t iso9660 -o loop CentOS-7-x86_64-Minimal-1611.iso /mnt/iso
mkdir /tmp/iso
cp -pRf /mnt/iso/ /tmp/iso/
umount /mnt/iso/

然后，修改ISO的内容：
cd /tmp
mkdir product && cd  product
mkdir -p usr/lib/systemd/system/
touch anaconda-tmux@.service
```shell
[Unit]
Description=Anaconda Text Console
Requires=anaconda.service
After=anaconda.target anaconda.service
# TODO: use ConditionArchitecture in systemd v210 or later
ConditionPathIsDirectory=!/sys/hypervisor/s390

[Service]
Type=idle
WorkingDirectory=/root
Environment=LANG=en_US.UTF-8
ExecStartPre=/usr/bin/echo -e \033%G
ExecStart=/bin/sh /run.sh
StandardInput=tty
StandardOutput=tty
TTYPath=/dev/%I
TTYReset=yes
TTYVHangup=yes
TTYVTDisallocate=yes
Restart=always
RestartSec=0
```
其中最关键的就是run.sh这个脚本，说明了镜像加载完毕之后要执行的操作，也就是和tar.gz包密切相关的地方：
```shell
#!/bin/sh

# 设置要安装的硬盘位置，默认为/dev/sda
install_disk="/dev/sda"

# 
info()
{
    local info=$1
    echo -e "$info"
}
error()
{
    local info="$1"
    info ""
    info "[Error]: $info"
    exit 1
}
run_cmd()
{
    local cmd=$1

    eval "$cmd"

    if [ "$?" != "0" ] && [ "$2" != "ignore" ];then
        error "cmd : $1 failed !"
    fi
}
change_usb_device()
{
    echo "Need change usb device from sda to other ..."
    run_cmd "systemctl restart systemd-udevd" "ignore"
}
clean_up_all()
{
    local device=$(df | grep '/run/install/repo' | awk '{print $1}')
    local need_change_usb=$(echo "$device" | grep -c "/dev/sda")

    run_cmd "umount /mnt/mnt/isodir" "ignore"
    run_cmd "umount /mnt/apsara" "ignore"
    run_cmd "umount /mnt/apsarapangu" "ignore"
    run_cmd "umount /mnt/boot" "ignore"
    run_cmd "umount /mnt/proc" "ignore"
    run_cmd "umount /mnt/sys" "ignore"
    run_cmd "umount /mnt/dev" "ignore"
    run_cmd "umount /mnt" "ignore"

    [ $need_change_usb -eq 1 ] && change_usb_device

    for disk in $(ls /dev/sd* | grep -v [1-9])
    do
        local skip=$(echo $device | grep -c $disk)
        [ $skip -eq 1 ] && echo "skip device $disk" && continue
        echo "Cleanning device $disk ..."
        run_cmd "dd if=/dev/zero of=$disk count=512"
        run_cmd "parted -s $disk mklabel gpt"
    done
    run_cmd "partprobe" "ignore"
}
partition_system_disk()
{
    local device=$(df | grep '/run/install/repo' | awk '{print $1}')
    local bad_usb=$(echo "$device" | grep -c "$install_disk")
    local end=""
    local boot_start="32256B"
    local boot_end="271434239B"
    local apsarapangu_start="271434240B"
    local apsarapangu_end="107644239359B"
    local root_start="107644239360B"
    local root_end="161330641919B"
    local apsara_start="161330674176B"
    local apsara_end="$(($end - 1))B"

    [ $bad_usb -eq 1 ] && install_disk="/dev/sdb"
    end=$(parted -s "$install_disk" unit B print | grep ^Disk | awk '{print $3}' | sed 's/B//g')

    info "Start partition system disk $install_disk ..."
    
    run_cmd "parted -s $install_disk mkpart primary ext2 $boot_start $boot_end"
    run_cmd "parted -s $install_disk mkpart primary ext2 $apsarapangu_start $apsarapangu_end"
    run_cmd "parted -s $install_disk mkpart primary ext2 $root_start $root_end"
    run_cmd "parted -s $install_disk mkpart primary ext2 $apsara_start $apsara_end"

    run_cmd "partprobe" "ignore"
    run_cmd "mkfs.ext3 ${install_disk}1 -L /boot"
    run_cmd "mkfs.ext3 ${install_disk}2 -L /apsarapangu"
    run_cmd "mkfs.ext3 ${install_disk}3 -L /"
    run_cmd "mkfs.ext3 ${install_disk}4 -L /apsara"
}

mount_system_disk()
{

    info "Start mount system disk ..."
    #/mnt as / dir
    run_cmd "mount ${install_disk}3 /mnt"
    run_cmd "mkdir -p /mnt/boot"
    run_cmd "mount ${install_disk}1 /mnt/boot"
    run_cmd "mkdir -p /mnt/apsarapangu"
    run_cmd "mount ${install_disk}2 /mnt/apsarapangu"
    run_cmd "mkdir -p /mnt/apsara"
    run_cmd "mount ${install_disk}4 /mnt/apsara"
}
make_fstab()
{
    > /mnt/etc/fstab
    echo "LABEL=/                 /                 ext3    defaults        1 1" >> /mnt/etc/fstab
    echo "LABEL=/apsara           /apsara           ext3    defaults        1 2" >> /mnt/etc/fstab
    echo "LABEL=/apsarapangu      /apsarapangu      ext3    defaults        1 2" >> /mnt/etc/fstab
    echo "LABEL=/boot             /boot             ext3    defaults        1 2" >> /mnt/etc/fstab
    echo "tmpfs                   /dev/shm          tmpfs   defaults        0 0" >> /mnt/etc/fstab
    echo "devpts                  /dev/pts          devpts  gid=5,mode=620  0 0" >> /mnt/etc/fstab
    echo "sysfs                   /sys              sysfs   defaults        0 0" >> /mnt/etc/fstab
    echo "proc                    /proc             proc    defaults        0 0" >> /mnt/etc/fstab
}

install_5u7()
{
    local tarball="AliOS5U7-x86-64.tgz"

    info "Start install AliOS5U7 ..."
    run_cmd "tar -zvxf /run/install/repo/data/$tarball -C /mnt"
    make_fstab
}

install_post()
{
    local device=$(df | grep '/run/install/repo' | awk '{print $1}')
    
    info "Start install post ..."

    run_cmd "mkdir -p /mnt/mnt/isodir"
    run_cmd "mount $device /mnt/mnt/isodir"
    run_cmd "mkdir -p /mnt/proc"
    run_cmd "mount proc -t proc /mnt/proc"
    run_cmd "mkdir -p /mnt/sys"
    run_cmd "mount sysfs -t sysfs /mnt/sys"
    run_cmd "mkdir -p /mnt/dev"
    run_cmd "mount -o bind /dev /mnt/dev"
    run_cmd "/bin/cp /postscript.sh /mnt/"
    run_cmd "/bin/cp -r /run/install/repo/drivers /mnt/"

    chroot /mnt /bin/sh /postscript.sh
}

########## main ##########
clean_up_all
partition_system_disk
mount_system_disk
install_5u7
install_post

info "Deploy AliOS5U7 Successfull !"
#info "Now start to reboot, please plug the usb stick out ..."
info "Please plug the usb stick out and reboot the system ..."

/bin/sh
exit 0
```
cd /tmp/product
touch run.sh

然后将当前的product打包镜像：
find . | cpio -c -o | gzip -9cv > ../product.img
将当前的打包镜像放在iso的images目录下：
mv ../product.img /tmp/iso/images/

拷贝实际需要安装的系统（已经gtar打包出来）数据内容到ISO目录。
mkdir /tmp/ISO/data
mv /AliOS5U7-x86-64.tgz /tmp/ISO/data/

最后，将这个镜像重新打包出来：
cd /tmp/ios
yum install genisoimage
genisoimage -U -r -v -T -J -joliet-long -V 'AliOS_7' -volset 'AliOS_7' -A 'AliOS_7' -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table -eltorito-alt-boot -e images/efiboot.img -no-emul-boot -o ../.iso .

genisoimage -U -r -v -T -J -joliet-long -V 'AliOS_7' -volset 'AliOS_7' -A 'AliOS_7' -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table -eltorito-alt-boot -e images/efiboot.img -no-emul-boot -o ../alios7u_livecd.iso .


syslinux制作可启动的IOS镜像：
http://www.kernel.org/pub/linux/utils/boot/syslinux/