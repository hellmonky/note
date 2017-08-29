<!-- TOC -->

- [在虚拟机中使用kolla部署openstack](#在虚拟机中使用kolla部署openstack)
    - [主机配置要求：](#主机配置要求)
    - [单机搭建流程：](#单机搭建流程)
        - [（0）设置当前host的网络：](#0设置当前host的网络)
        - [（1）修改主机名：](#1修改主机名)
        - [（2）关闭firewalld和iptables：](#2关闭firewalld和iptables)
        - [（3）重启，然后安装repl源：](#3重启然后安装repl源)
        - [（4）安装基础软件：](#4安装基础软件)
        - [（5）安装并且设置docker：](#5安装并且设置docker)
        - [（6）安装Ansible：](#6安装ansible)
        - [（7）搭建Registry服务器：](#7搭建registry服务器)
        - [（8）下载kolla官方提供的ocata镜像：](#8下载kolla官方提供的ocata镜像)
        - [（9）安装kolla-ansible：](#9安装kolla-ansible)
        - [（10）安装kolla：](#10安装kolla)
        - [（11）验证部署：](#11验证部署)
        - [（12）安装安装OpenStack client端](#12安装安装openstack-client端)
        - [（13）创建网络：](#13创建网络)
        - [（14）进入dashboard创建虚拟机，绑定浮动ip：](#14进入dashboard创建虚拟机绑定浮动ip)
        - [（15）检查虚拟网络：](#15检查虚拟网络)
        - [（16）KVM嵌套虚拟化：](#16kvm嵌套虚拟化)
        - [（17）迁移虚拟机：](#17迁移虚拟机)
        - [（18）使用libvirt管理虚拟机：](#18使用libvirt管理虚拟机)
    - [OpenStack的命令行工具使用：](#openstack的命令行工具使用)
    - [OpenStack的编程开发：](#openstack的编程开发)

<!-- /TOC -->

# 在虚拟机中使用kolla部署openstack

我果然变成了运维~

## 主机配置要求：
哪一个标准的centos7_x86-64镜像，添加两块网卡。
保证外网通信正常，一切主机的准备工作就完毕了。

## 单机搭建流程：

### （0）设置当前host的网络：
yum install net-tools
vi /etc/sysconfig/network-script/ifcfg-exx
重启后可以正常联网
### （1）修改主机名：
hostnamectl set-hostname kolla
hostnamectl --pretty
hostnamectl --static
### （2）关闭firewalld和iptables：
systemctl disable firewalld
systemctl stop firewalld
systemctl status firewalld
sed -i "s/SELINUX=enforcing/SELINUX=disabled/" /etc/selinux/config
### （3）重启，然后安装repl源：
reboot
yum install epel-release -y
注意：如果使用yum clean all清理过，还需要重新update一下，建立缓存，不然后续无法下载。
### （4）安装基础软件：
 yum install python-devel libffi-devel gcc openssl-devel git python-pip -y
### （5）安装并且设置docker：
curl -sSL https://get.docker.com/ | sh
sudo usermod -aG docker root
mkdir /etc/systemd/system/docker.service.d
tee /etc/systemd/system/docker.service.d/kolla.conf << 'EOF'
[Service]
MountFlags=shared
EOF
然后重启服务：
systemctl daemon-reload
systemctl enable docker
systemctl restart docker
systemctl status docker
访问私有的Docker仓库：
查看网卡的ip：
```shell
ip a|grep ens3 |grep inet|awk '{print $2}'|cut -d/ -f1
```
编辑/usr/lib/systemd/system/docker.service：
```shell
#ExecStart=/usr/bin/dockerd
ExecStart=/usr/bin/dockerd --insecure-registry 192.168.122.142:4000
```
重启服务
systemctl daemon-reload
systemctl restart docker
### （6）安装Ansible：
yum install ansible
### （7）搭建Registry服务器：
默认docker的registry是使用5000端口，对于OpenStack来说，有端口冲突，所以我将端口改成了4000：
docker run -d -v /opt/registry:/var/lib/registry -p 4000:5000 \
--restart=always --name registry registry:2
### （8）下载kolla官方提供的ocata镜像：
wget http://tarballs.openstack.org/kolla/images/centos-source-registry-ocata.tar.gz
du -sh centos-source-registry-ocata.tar.gz 
tar zxf centos-source-registry-ocata.tar.gz -C /opt/registry/
因为这个镜像比较大，而且考虑到qcow2删除文件后，镜像并不会减小，所以可以先在主机外下载好，然后将远程tgz直接解压到本地，避免反复传输文件占用空间：
cd /opt/registry/
ssh root@192.168.122.1 "cat /home/wentao/workspace/kolla/centos-source-registry-ocata.tar.gz" | tar zxvf -
### （9）安装kolla-ansible：
git clone http://git.trystack.cn/openstack/kolla-ansible -b stable/ocata
cd kolla-ansible/
pip install . -i https://pypi.tuna.tsinghua.edu.cn/simple
如果pip速度很慢，后面可以加上参数-i https://pypi.tuna.tsinghua.edu.cn/simple，指定国内的pip源。
cp -r etc/kolla /etc/kolla/
如果是在虚拟机里装kolla，希望可以启动再启动虚拟机，那么你需要把virt_type=qemu，默认是kvm（首先确认物理机打开了嵌套kvm的支持）：
mkdir -p /etc/kolla/config/nova
cat << EOF > /etc/kolla/config/nova/nova-compute.conf
[libvirt]
virt_type=qemu
cpu_mode = none
EOF
### （10）安装kolla：
生成密码文件
kolla-genpwd
编辑/etc/kolla/passwords.yml
cp /etc/kolla/passwords.yml /etc/kolla/passwords.yml.bak
vi cp /etc/kolla/passwords.yml
```shell
# vim /etc/kolla/passwords.yml
keystone_admin_password: admin
```
这是登录Dashboard，admin使用的密码。
编辑/etc/kolla/globals.yml文件
cp /etc/kolla/globals.yml /etc/kolla/globals.yml.bak
vim /etc/kolla/globals.yml
```shell
kolla_base_distro: "centos"
kolla_install_type: "source"
openstack_release: "4.0.3"
kolla_internal_vip_address: "192.168.122.100"
docker_registry: "192.168.122.142:4000"
docker_namespace: "lokolla"
network_interface: "ens3"
neutron_external_interface: "ens8"
```
PS：需要注意的是这儿的网络设置和上面的docker设置中的网络相关。
如果不知道openstack_release和docker_namespace，可以进入：
/opt/registry/docker/registry/v2/repositories/lokolla/centos-source-keepalived/_manifests/tags/4.0.3/
就可以知道openstack_release为4.0.3和docker_namespace为lokolla了。
### （11）验证部署：
验证：
kolla-ansible prechecks -i kolla-ansible/ansible/inventory/all-in-one
pull镜像：
kolla-ansible pull -i kolla-ansible/ansible/inventory/all-in-one
完毕后，查看当前的镜像有：
```shell
[root@kolla lokolla]# docker images
REPOSITORY                                                             TAG                 IMAGE ID            CREATED             SIZE
192.168.122.142:4000/lokolla/centos-source-keystone                    4.0.3               4b2a0ef62d50        17 hours ago        885MB
192.168.122.142:4000/lokolla/centos-source-heat-api                    4.0.3               c97afd9bc296        17 hours ago        818MB
192.168.122.142:4000/lokolla/centos-source-heat-api-cfn                4.0.3               72f6fbdfc4c0        17 hours ago        818MB
192.168.122.142:4000/lokolla/centos-source-heat-engine                 4.0.3               eade01736e76        17 hours ago        818MB
192.168.122.142:4000/lokolla/centos-source-glance-api                  4.0.3               8df1283c5496        17 hours ago        914MB
192.168.122.142:4000/lokolla/centos-source-glance-registry             4.0.3               2ab03152cb72        18 hours ago        856MB
192.168.122.142:4000/lokolla/centos-source-neutron-server              4.0.3               ecd6a4bf057c        18 hours ago        915MB
192.168.122.142:4000/lokolla/centos-source-neutron-metadata-agent      4.0.3               4d511c747996        18 hours ago        907MB
192.168.122.142:4000/lokolla/centos-source-neutron-dhcp-agent          4.0.3               8e3751de176b        18 hours ago        907MB
192.168.122.142:4000/lokolla/centos-source-neutron-l3-agent            4.0.3               8e3751de176b        18 hours ago        907MB
192.168.122.142:4000/lokolla/centos-source-neutron-openvswitch-agent   4.0.3               8e3751de176b        18 hours ago        907MB
192.168.122.142:4000/lokolla/centos-source-nova-compute                4.0.3               4b3c48f866be        18 hours ago        1.21GB
192.168.122.142:4000/lokolla/centos-source-nova-api                    4.0.3               41725610b3a2        18 hours ago        1.05GB
192.168.122.142:4000/lokolla/centos-source-nova-placement-api          4.0.3               76fc57f3e48c        18 hours ago        1.05GB
192.168.122.142:4000/lokolla/centos-source-nova-ssh                    4.0.3               c64f31da8ecc        18 hours ago        1.02GB
192.168.122.142:4000/lokolla/centos-source-nova-conductor              4.0.3               c659d08eb2aa        18 hours ago        975MB
192.168.122.142:4000/lokolla/centos-source-nova-consoleauth            4.0.3               c659d08eb2aa        18 hours ago        975MB
192.168.122.142:4000/lokolla/centos-source-nova-scheduler              4.0.3               c659d08eb2aa        18 hours ago        975MB
192.168.122.142:4000/lokolla/centos-source-nova-novncproxy             4.0.3               31b6aefa5f3e        18 hours ago        1GB
192.168.122.142:4000/lokolla/centos-source-openvswitch-vswitchd        4.0.3               b650cf2bb01b        18 hours ago        440MB
192.168.122.142:4000/lokolla/centos-source-openvswitch-db-server       4.0.3               ee462c1411e1        18 hours ago        440MB
192.168.122.142:4000/lokolla/centos-source-nova-libvirt                4.0.3               bbdc27dcdb9a        18 hours ago        971MB
192.168.122.142:4000/lokolla/centos-source-fluentd                     4.0.3               54f4bae3cab5        18 hours ago        710MB
192.168.122.142:4000/lokolla/centos-source-rabbitmq                    4.0.3               fd59d9530bcb        18 hours ago        478MB
192.168.122.142:4000/lokolla/centos-source-memcached                   4.0.3               592e2c3f1f26        18 hours ago        419MB
192.168.122.142:4000/lokolla/centos-source-mariadb                     4.0.3               bcaa069fb51f        18 hours ago        810MB
192.168.122.142:4000/lokolla/centos-source-kolla-toolbox               4.0.3               00e07eb1a165        18 hours ago        738MB
192.168.122.142:4000/lokolla/centos-source-haproxy                     4.0.3               1751ff4f59c4        18 hours ago        439MB
192.168.122.142:4000/lokolla/centos-source-cron                        4.0.3               03bb5a21e7bf        18 hours ago        418MB
192.168.122.142:4000/lokolla/centos-source-keepalived                  4.0.3               8415d9f13d8e        18 hours ago        423MB
registry                                                               2                   751f286bc25e        4 weeks ago         33.2MB
```
部署：
kolla-ansible deploy -i kolla-ansible/ansible/inventory/all-in-one
最终验证：
kolla-ansible post-deploy
这样就创建 /etc/kolla/admin-openrc.sh文件。
### （12）安装安装OpenStack client端
pip install python-openstackclient
这样就可以访问：
http://192.168.122.175/auth/login
账号：admin
密码：admin
就可以登入，进入管理界面了。
### （13）创建网络：
编辑 /usr/share/kolla-ansible/init-runonce
网络需要根据实际情况修改,我都是用的nat模式：
EXT_NET_CIDR='192.168.122.0/24'
EXT_NET_RANGE='start=192.168.122.10,end=192.168.122.20'
EXT_NET_GATEWAY='192.168.122.2'
运行脚本创建
source /etc/kolla/admin-openrc.sh
bash /usr/share/kolla-ansible/init-runonce
在运行中，会要求输入ssh的密码，默认回车为空就可以了。
最终的输出结果为：
```shell
Downloading glance image.
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 12.6M  100 12.6M    0     0  48875      0  0:04:31  0:04:31 --:--:--  402k
Creating glance image.
+------------------+------------------------------------------------------+
| Field            | Value                                                |
+------------------+------------------------------------------------------+
| checksum         | ee1eca47dc88f4879d8a229cc70a07c6                     |
| container_format | bare                                                 |
| created_at       | 2017-08-24T12:08:00Z                                 |
| disk_format      | qcow2                                                |
| file             | /v2/images/52e3ec79-d39c-413c-b2d8-2fb51fff49c5/file |
| id               | 52e3ec79-d39c-413c-b2d8-2fb51fff49c5                 |
| min_disk         | 0                                                    |
| min_ram          | 0                                                    |
| name             | cirros                                               |
| owner            | c04b9155ef0f4469ac55b9cd2acf2b95                     |
| protected        | False                                                |
| schema           | /v2/schemas/image                                    |
| size             | 13287936                                             |
| status           | active                                               |
| tags             |                                                      |
| updated_at       | 2017-08-24T12:08:01Z                                 |
| virtual_size     | None                                                 |
| visibility       | public                                               |
+------------------+------------------------------------------------------+
Configuring neutron.
+---------------------------+--------------------------------------+
| Field                     | Value                                |
+---------------------------+--------------------------------------+
| admin_state_up            | UP                                   |
| availability_zone_hints   |                                      |
| availability_zones        |                                      |
| created_at                | 2017-08-24T12:08:04Z                 |
| description               |                                      |
| dns_domain                | None                                 |
| id                        | b933af32-a2c4-437c-85b2-1aef835f6d60 |
| ipv4_address_scope        | None                                 |
| ipv6_address_scope        | None                                 |
| is_default                | False                                |
| is_vlan_transparent       | None                                 |
| mtu                       | 1500                                 |
| name                      | public1                              |
| port_security_enabled     | False                                |
| project_id                | c04b9155ef0f4469ac55b9cd2acf2b95     |
| provider:network_type     | flat                                 |
| provider:physical_network | physnet1                             |
| provider:segmentation_id  | None                                 |
| qos_policy_id             | None                                 |
| revision_number           | 3                                    |
| router:external           | External                             |
| segments                  | None                                 |
| shared                    | False                                |
| status                    | ACTIVE                               |
| subnets                   |                                      |
| tags                      |                                      |
| updated_at                | 2017-08-24T12:08:05Z                 |
+---------------------------+--------------------------------------+
+-------------------------+--------------------------------------+
| Field                   | Value                                |
+-------------------------+--------------------------------------+
| allocation_pools        | 192.168.122.10-192.168.122.20        |
| cidr                    | 192.168.122.0/24                     |
| created_at              | 2017-08-24T12:08:08Z                 |
| description             |                                      |
| dns_nameservers         |                                      |
| enable_dhcp             | False                                |
| gateway_ip              | 192.168.122.2                        |
| host_routes             |                                      |
| id                      | 7f61e9ab-8592-4270-9bb5-3091c6d094c0 |
| ip_version              | 4                                    |
| ipv6_address_mode       | None                                 |
| ipv6_ra_mode            | None                                 |
| name                    | public1-subnet                       |
| network_id              | b933af32-a2c4-437c-85b2-1aef835f6d60 |
| project_id              | c04b9155ef0f4469ac55b9cd2acf2b95     |
| revision_number         | 2                                    |
| segment_id              | None                                 |
| service_types           |                                      |
| subnetpool_id           | None                                 |
| tags                    |                                      |
| updated_at              | 2017-08-24T12:08:08Z                 |
| use_default_subnet_pool | None                                 |
+-------------------------+--------------------------------------+
+---------------------------+--------------------------------------+
| Field                     | Value                                |
+---------------------------+--------------------------------------+
| admin_state_up            | UP                                   |
| availability_zone_hints   |                                      |
| availability_zones        |                                      |
| created_at                | 2017-08-24T12:08:11Z                 |
| description               |                                      |
| dns_domain                | None                                 |
| id                        | 60ff4cc9-22a5-485b-ac31-3a07731caaba |
| ipv4_address_scope        | None                                 |
| ipv6_address_scope        | None                                 |
| is_default                | None                                 |
| is_vlan_transparent       | None                                 |
| mtu                       | 1450                                 |
| name                      | demo-net                             |
| port_security_enabled     | False                                |
| project_id                | c04b9155ef0f4469ac55b9cd2acf2b95     |
| provider:network_type     | vxlan                                |
| provider:physical_network | None                                 |
| provider:segmentation_id  | 42                                   |
| qos_policy_id             | None                                 |
| revision_number           | 2                                    |
| router:external           | Internal                             |
| segments                  | None                                 |
| shared                    | False                                |
| status                    | ACTIVE                               |
| subnets                   |                                      |
| tags                      |                                      |
| updated_at                | 2017-08-24T12:08:11Z                 |
+---------------------------+--------------------------------------+
+-------------------------+--------------------------------------+
| Field                   | Value                                |
+-------------------------+--------------------------------------+
| allocation_pools        | 10.0.0.2-10.0.0.254                  |
| cidr                    | 10.0.0.0/24                          |
| created_at              | 2017-08-24T12:08:13Z                 |
| description             |                                      |
| dns_nameservers         | 8.8.8.8                              |
| enable_dhcp             | True                                 |
| gateway_ip              | 10.0.0.1                             |
| host_routes             |                                      |
| id                      | 72fdc05f-3bdc-4933-8059-83b7acfc617f |
| ip_version              | 4                                    |
| ipv6_address_mode       | None                                 |
| ipv6_ra_mode            | None                                 |
| name                    | demo-subnet                          |
| network_id              | 60ff4cc9-22a5-485b-ac31-3a07731caaba |
| project_id              | c04b9155ef0f4469ac55b9cd2acf2b95     |
| revision_number         | 2                                    |
| segment_id              | None                                 |
| service_types           |                                      |
| subnetpool_id           | None                                 |
| tags                    |                                      |
| updated_at              | 2017-08-24T12:08:14Z                 |
| use_default_subnet_pool | None                                 |
+-------------------------+--------------------------------------+
+-------------------------+--------------------------------------+
| Field                   | Value                                |
+-------------------------+--------------------------------------+
| admin_state_up          | UP                                   |
| availability_zone_hints |                                      |
| availability_zones      |                                      |
| created_at              | 2017-08-24T12:08:18Z                 |
| description             |                                      |
| distributed             | False                                |
| external_gateway_info   | None                                 |
| flavor_id               | None                                 |
| ha                      | False                                |
| id                      | 6afc820b-6dd6-4787-8fee-eccf6e235a0d |
| name                    | demo-router                          |
| project_id              | c04b9155ef0f4469ac55b9cd2acf2b95     |
| revision_number         | None                                 |
| routes                  |                                      |
| status                  | ACTIVE                               |
| tags                    |                                      |
| updated_at              | 2017-08-24T12:08:18Z                 |
+-------------------------+--------------------------------------+
+-------------------+--------------------------------------+
| Field             | Value                                |
+-------------------+--------------------------------------+
| created_at        | 2017-08-24T12:09:26Z                 |
| description       |                                      |
| direction         | ingress                              |
| ether_type        | IPv4                                 |
| id                | 161bfd46-1dc3-45b5-94ce-19194293ad97 |
| name              | None                                 |
| port_range_max    | None                                 |
| port_range_min    | None                                 |
| project_id        | c04b9155ef0f4469ac55b9cd2acf2b95     |
| protocol          | icmp                                 |
| remote_group_id   | None                                 |
| remote_ip_prefix  | 0.0.0.0/0                            |
| revision_number   | 1                                    |
| security_group_id | a199d7a8-60b5-4b3b-b3d4-bd39d199edc3 |
| updated_at        | 2017-08-24T12:09:26Z                 |
+-------------------+--------------------------------------+
+-------------------+--------------------------------------+
| Field             | Value                                |
+-------------------+--------------------------------------+
| created_at        | 2017-08-24T12:09:31Z                 |
| description       |                                      |
| direction         | ingress                              |
| ether_type        | IPv4                                 |
| id                | e0ff96dc-93ab-41f1-9c12-17a623287b20 |
| name              | None                                 |
| port_range_max    | 22                                   |
| port_range_min    | 22                                   |
| project_id        | c04b9155ef0f4469ac55b9cd2acf2b95     |
| protocol          | tcp                                  |
| remote_group_id   | None                                 |
| remote_ip_prefix  | 0.0.0.0/0                            |
| revision_number   | 1                                    |
| security_group_id | a199d7a8-60b5-4b3b-b3d4-bd39d199edc3 |
| updated_at        | 2017-08-24T12:09:31Z                 |
+-------------------+--------------------------------------+
+-------------------+--------------------------------------+
| Field             | Value                                |
+-------------------+--------------------------------------+
| created_at        | 2017-08-24T12:09:37Z                 |
| description       |                                      |
| direction         | ingress                              |
| ether_type        | IPv4                                 |
| id                | 021468d3-55f4-417c-a4dd-b0c2c99bb763 |
| name              | None                                 |
| port_range_max    | 8000                                 |
| port_range_min    | 8000                                 |
| project_id        | c04b9155ef0f4469ac55b9cd2acf2b95     |
| protocol          | tcp                                  |
| remote_group_id   | None                                 |
| remote_ip_prefix  | 0.0.0.0/0                            |
| revision_number   | 1                                    |
| security_group_id | a199d7a8-60b5-4b3b-b3d4-bd39d199edc3 |
| updated_at        | 2017-08-24T12:09:37Z                 |
+-------------------+--------------------------------------+
+-------------------+--------------------------------------+
| Field             | Value                                |
+-------------------+--------------------------------------+
| created_at        | 2017-08-24T12:09:41Z                 |
| description       |                                      |
| direction         | ingress                              |
| ether_type        | IPv4                                 |
| id                | 08d95720-476f-4a28-80d4-84c077c90960 |
| name              | None                                 |
| port_range_max    | 8080                                 |
| port_range_min    | 8080                                 |
| project_id        | c04b9155ef0f4469ac55b9cd2acf2b95     |
| protocol          | tcp                                  |
| remote_group_id   | None                                 |
| remote_ip_prefix  | 0.0.0.0/0                            |
| revision_number   | 1                                    |
| security_group_id | a199d7a8-60b5-4b3b-b3d4-bd39d199edc3 |
| updated_at        | 2017-08-24T12:09:41Z                 |
+-------------------+--------------------------------------+
Generating ssh key.
Generating public/private rsa key pair.
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /root/.ssh/id_rsa.
Your public key has been saved in /root/.ssh/id_rsa.pub.
The key fingerprint is:
66:18:38:96:55:79:e8:92:3b:d9:14:3c:55:63:1d:e5 root@kolla
The key's randomart image is:
+--[ RSA 2048]----+
|      .o.+..+..o.|
|     +  * .. ... |
|    = .o +      E|
|   . .ooo        |
|      .*S        |
|      +o.        |
|       .         |
|                 |
|                 |
+-----------------+
Configuring nova public key and quotas.
+-------------+-------------------------------------------------+
| Field       | Value                                           |
+-------------+-------------------------------------------------+
| fingerprint | 66:18:38:96:55:79:e8:92:3b:d9:14:3c:55:63:1d:e5 |
| name        | mykey                                           |
| user_id     | 9d87430be50440b0a040d1e71179e5f5                |
+-------------+-------------------------------------------------+
+----------------------------+---------+
| Field                      | Value   |
+----------------------------+---------+
| OS-FLV-DISABLED:disabled   | False   |
| OS-FLV-EXT-DATA:ephemeral  | 0       |
| disk                       | 1       |
| id                         | 1       |
| name                       | m1.tiny |
| os-flavor-access:is_public | True    |
| properties                 |         |
| ram                        | 512     |
| rxtx_factor                | 1.0     |
| swap                       |         |
| vcpus                      | 1       |
+----------------------------+---------+
+----------------------------+----------+
| Field                      | Value    |
+----------------------------+----------+
| OS-FLV-DISABLED:disabled   | False    |
| OS-FLV-EXT-DATA:ephemeral  | 0        |
| disk                       | 20       |
| id                         | 2        |
| name                       | m1.small |
| os-flavor-access:is_public | True     |
| properties                 |          |
| ram                        | 2048     |
| rxtx_factor                | 1.0      |
| swap                       |          |
| vcpus                      | 1        |
+----------------------------+----------+
+----------------------------+-----------+
| Field                      | Value     |
+----------------------------+-----------+
| OS-FLV-DISABLED:disabled   | False     |
| OS-FLV-EXT-DATA:ephemeral  | 0         |
| disk                       | 40        |
| id                         | 3         |
| name                       | m1.medium |
| os-flavor-access:is_public | True      |
| properties                 |           |
| ram                        | 4096      |
| rxtx_factor                | 1.0       |
| swap                       |           |
| vcpus                      | 2         |
+----------------------------+-----------+
+----------------------------+----------+
| Field                      | Value    |
+----------------------------+----------+
| OS-FLV-DISABLED:disabled   | False    |
| OS-FLV-EXT-DATA:ephemeral  | 0        |
| disk                       | 80       |
| id                         | 4        |
| name                       | m1.large |
| os-flavor-access:is_public | True     |
| properties                 |          |
| ram                        | 8192     |
| rxtx_factor                | 1.0      |
| swap                       |          |
| vcpus                      | 4        |
+----------------------------+----------+
+----------------------------+-----------+
| Field                      | Value     |
+----------------------------+-----------+
| OS-FLV-DISABLED:disabled   | False     |
| OS-FLV-EXT-DATA:ephemeral  | 0         |
| disk                       | 160       |
| id                         | 5         |
| name                       | m1.xlarge |
| os-flavor-access:is_public | True      |
| properties                 |           |
| ram                        | 16384     |
| rxtx_factor                | 1.0       |
| swap                       |           |
| vcpus                      | 8         |
+----------------------------+-----------+

Done.

To deploy a demo instance, run:

openstack server create \
    --image cirros \
    --flavor m1.tiny \
    --key-name mykey \
    --nic net-id=60ff4cc9-22a5-485b-ac31-3a07731caaba \
    demo1
```
### （14）进入dashboard创建虚拟机，绑定浮动ip：
进入dashboard创建与主机即可，网络选择demo-net网络，创建完成后标定浮动ip即可。
这部分就是在创建虚拟机了，在dashboard中进行配置就可以了。
默认只有一个cirros模板，用他来创建的虚机，用户名为：cirros，密码默认为：cubswin:)
sshpass -p cubswin:) ssh cirros@192.168.122.18
就可以进入查看当前创建的虚拟机了。
默认的这个模板为了尽量小，在bin目录下使用了busybox，可以使用：
busybox --list
查看支持的命令，然后使用：
busybox ifconfig
查看当前的网络配置。
进入/boot查看内容为：
```shell
$ ls -l
total 8486
-rw-r--r--    1 root     root        140805 Mar 23  2015 config-3.2.0-80-virtual
drwxr-xr-x    2 root     root          1024 May  7  2015 grub
-rw-r--r--    1 root     root       3530665 May  7  2015 initrd.img-3.2.0-80-virtual
-rw-------    1 root     root       4979632 Mar 23  2015 vmlinuz-3.2.0-80-virtual
```
### （15）检查虚拟网络：
如果上一步中没有绑定浮动IP，这个查看是没有结果的。
openstack server list
如果回显：
```shell
Missing value auth-url required for auth plugin password
```
需要将之前的export的内容重新载入一下：
source /etc/kolla/admin-openrc.sh
然后再次执行：
openstack server list
回显：
```shell
+--------------------------------------+------+--------+------------------------------------+--------+---------+
| ID                                   | Name | Status | Networks                           | Image  | Flavor  |
+--------------------------------------+------+--------+------------------------------------+--------+---------+
| 94c5b076-9319-4666-9253-7e1877d94daa | test | ACTIVE | demo-net=10.0.0.11, 192.168.122.18 | cirros | m1.tiny |
+--------------------------------------+------+--------+------------------------------------+--------+---------+
```
可以看到当前创建的虚拟机的基本信息，包含网络信息。
### （16）KVM嵌套虚拟化：
openstack默认是kvm嵌套虚拟化支持的，但是在之前的设置中，已经修改为qemu的软虚拟化。
首先检查当前的物理主机是否支持嵌套虚拟化：
cat /sys/module/kvm_intel/parameters/nested
如果返回是Y，就表示支持（默认已经打开，否则看其他嵌套虚拟化的打开方式教程）。
在ubuntu上，嵌套虚拟化的打开配置放在：
/etc/modprobe.d/qemu-system-x86.conf
内容为：
```shell
options kvm_intel nested=1
```
同时，如果要测试KVM中嵌套KVM，就需要将当前openstack的nova设置修改回去：
/etc/kolla/config/nova/nova-compute.conf
将其中的qemu修改为kvm就可以了。
然后重启Nova服务，新建的虚拟机就是嵌套的kvm虚机了。
### （17）迁移虚拟机：
将当前测试完毕的kolla虚拟机进行迁移，放在服务器上实际运行。
那么就需要将当前的这个虚拟机配置文件找到，然后用这个配置文件和镜像来完成迁移。进入：
/etc/libvirt/qemu/
就可以看到在virt-manager中命名的虚拟机的xml配置文件，拷贝出来就可以了。
然后就可以用制作好的镜像和xml配置文件来创建并启动虚拟机：
virsh define XXX.xml 
virsh start XXX_NAME
其中的 XXX_NAME 就是在 XXX.xml中定义的虚拟机的名称。
测试完毕后，可以使用：
virsh list
查看当前正在运行的虚拟机。如果要关闭，可以使用：
virsh shutdown XXX_NAME
关闭虚拟机。
### （18）使用libvirt管理虚拟机：
常规情况下，安装完 KVM 之后，可能都会通过 VNC 连接到 KVM 虚拟机里面去设置相应的 IP 等信息。但是这样子，一方面可能会因为打开过多的端口造成安全问题，另一方面也不是会便捷。针对此种情况，我们可以使用 KVM 为我们提供的 console 接口功能，它可以采用字符界面进行 linux 虚拟机控制台连接。这样子，及时 KVM 虚拟机没有 IP 地址，又或者 KVM 虚拟机出现了问题通过 IP 连接不进去了，都可以很便捷的快速进入到 KVM 虚拟机里面去排查问题。
在远程服务器上也存在这个问题，如果KVM虚机的VNC端口没有打开，网络也没有设置通过，那么就需要通过SSH到远程服务器，然后virsh来进入虚机，设置好网络之后，再SSH进入进行管理。
这样也说明，只需要在远程服务器安装好libvirt就可以了，本地只需要SSH上去就可以执行操作。

打开KVM虚机的console支持：
> - 当前以centos7为例进行说明，具体的修改方法因系统不同存在差异。

（1）由于 /etc/securetty 文件允许你规定 root 用户可以从哪个 TTY 设备登录，因此我们需要添加 ttyS0 的安全许可，即将 ttyS0 添加至该文件，来允许我们的 root 用户登录。
echo "ttyS0" >> /etc/securetty
在centos7中，已经默认添加。
（2）通过在 /etc/inittab 里加一个 ttyS0 ，来使得系统启动时能够生成一个 ttyS0 来接收来自内核的数据：
vim /etc/inittab
在文件的最后添加如下内容：
S0:12345:respawn:/sbin/agetty ttyS0 115200
（3）通过为内核传递参数 console=ttyS0，来让内核把输出定向至 ttyS0：
grubby --update-kernel=ALL --args="console=ttyS0"
重启虚机后生效。
（4）使用virsh console来进行连接：
virsh console instance001
然后等待：
```shell
连接到域 centos7_base
换码符为 ^]
```
然后回车，进入系统，输入密码。
（5）如果需要退出，输入：
Ctrl+]

参考文档：
> - [配置KVM宿主机使用vish console命令直接进入CentOS6虚拟机](http://blog.csdn.net/oyym_mv/article/details/52412797)
> - [Virsh Console CentOS7](https://paulmellorsblog.wordpress.com/2015/01/23/virsh-console-centos7/)

如果在本地也安装了libvirt，也可以通过他来对远程的kvm虚机进行管理：
virsh --connect qemu+ssh://root@10.20.190.3/system list --all





参考文档：
> - [利用kolla快速搭建openstack-ocata单节点](https://www.lijiawang.org/posts/kolla-openstack-ocata.html)：
本文就是按照这个文档，在KVM的centos7_x86-64上面完成了部署测试。

> - [陈沙克日志](http://www.chenshake.com/)：
这位是kolla的开发团队人员，可以通过开发者的视角多了解一下。


## OpenStack的命令行工具使用：
使用：
pip install python-openstackclient
默认安装完毕后，就可以在浏览器中使用dashboard进行管理了。
但是很多时候命令行工具能给出更快速和自由的交互方式，本质上dashboard也是通过openstackclient来进行调用和操作的。
根据[OpenStackClient官方文档](https://docs.openstack.org/python-openstackclient/latest/index.html)，可以进行调用测试。
并且对于计算资源，可以使用nova来进行管理和创建。




## OpenStack的编程开发：
这儿使用OpenStack的python库来进行开发。
```shell
python-keystoneclient
python-glanceclient
python-novaclient
python-quantumclient
python-cinderclient
python-swiftclient
```
安装完毕之后，查看当前openstakc的入口地址：
```shell
openstack endpoint list
+----------------------------------+-----------+--------------+----------------+---------+-----------+------------------------------------------------+
| ID                               | Region    | Service Name | Service Type   | Enabled | Interface | URL                                            |
+----------------------------------+-----------+--------------+----------------+---------+-----------+------------------------------------------------+
| 161aa03fc28b45d8a6928fcbb753cee0 | RegionOne | neutron      | network        | True    | internal  | http://192.168.122.100:9696                    |
| 2349cac6df434fdba502f561107c18bb | RegionOne | heat         | orchestration  | True    | internal  | http://192.168.122.100:8004/v1/%(tenant_id)s   |
| 2bb403d3de094524a1211f704224e8fa | RegionOne | glance       | image          | True    | public    | http://192.168.122.100:9292                    |
| 31299bf472264b24bd3876666c6b13e9 | RegionOne | glance       | image          | True    | admin     | http://192.168.122.100:9292                    |
| 3349396ee2634075b2fb228f6f7f78ae | RegionOne | nova_legacy  | compute_legacy | True    | internal  | http://192.168.122.100:8774/v2/%(tenant_id)s   |
| 3bcce02b97824046a5df683fd43afec7 | RegionOne | nova         | compute        | True    | public    | http://192.168.122.100:8774/v2.1/%(tenant_id)s |
| 40f8f4202bbc42a18784d0c3832440f5 | RegionOne | placement    | placement      | True    | public    | http://192.168.122.100:8780                    |
| 42a0ada41b764a0095ae779d015cd930 | RegionOne | placement    | placement      | True    | internal  | http://192.168.122.100:8780                    |
| 4c2bdc52bc294afea5dace9a2a7e3ed3 | RegionOne | heat-cfn     | cloudformation | True    | public    | http://192.168.122.100:8000/v1                 |
| 6ba1a237771b41559fb40d0a03afcc43 | RegionOne | neutron      | network        | True    | public    | http://192.168.122.100:9696                    |
| 6d158ec24d3a4e88a2d3ea609989ce17 | RegionOne | heat-cfn     | cloudformation | True    | internal  | http://192.168.122.100:8000/v1                 |
| 7a450afba3284b7798e96555cdd55bbb | RegionOne | nova         | compute        | True    | admin     | http://192.168.122.100:8774/v2.1/%(tenant_id)s |
| 981d1525c635455a81cd01360b180963 | RegionOne | nova_legacy  | compute_legacy | True    | admin     | http://192.168.122.100:8774/v2/%(tenant_id)s   |
| af831d0b1f7d49168dd34bd4c31c1582 | RegionOne | keystone     | identity       | True    | public    | http://192.168.122.100:5000/v3                 |
| b398aa78ab554cd39e3ae0c6d83c86eb | RegionOne | nova         | compute        | True    | internal  | http://192.168.122.100:8774/v2.1/%(tenant_id)s |
| b61826aeefcd4b4e8d4554e8fe2390c8 | RegionOne | placement    | placement      | True    | admin     | http://192.168.122.100:8780                    |
| bbf369e7e0ab41fc9067e3892c8528ee | RegionOne | heat         | orchestration  | True    | admin     | http://192.168.122.100:8004/v1/%(tenant_id)s   |
| c2c2c6a56bfa4157a8a1fdbff965505c | RegionOne | keystone     | identity       | True    | admin     | http://192.168.122.100:35357/v3                |
| d289ef07783b47c7a5d14344f1736554 | RegionOne | glance       | image          | True    | internal  | http://192.168.122.100:9292                    |
| dd0143e0a7704638ba8683d751b09841 | RegionOne | keystone     | identity       | True    | internal  | http://192.168.122.100:5000/v3                 |
| e128a9b245214088a75805064b07ea94 | RegionOne | nova_legacy  | compute_legacy | True    | public    | http://192.168.122.100:8774/v2/%(tenant_id)s   |
| e37fadd15e864b7ead54f7b63dab4c29 | RegionOne | heat-cfn     | cloudformation | True    | admin     | http://192.168.122.100:8000/v1                 |
| fb012310791648bc85bcee4c022cfd0f | RegionOne | neutron      | network        | True    | admin     | http://192.168.122.100:9696                    |
| fe7e389e1c934ccd8f9e1d8b6b502b81 | RegionOne | heat         | orchestration  | True    | public    | http://192.168.122.100:8004/v1/%(tenant_id)s   |
+----------------------------------+-----------+--------------+----------------+---------+-----------+------------------------------------------------+
```
以nova为例，调用方式：
```python
import os
import time
from novaclient import client
 
nova = client.Client('2.1', 'admin','admin','test','http://192.168.122.100:5000/v3')
print nova.servers.list()
```
其中的url就是授权url，对应上述表中的keystone地址。
参考文档：
[python-novaclient官方文档](https://github.com/openstack/python-novaclient/blob/master/novaclient/client.py)
[The novaclient Python API](https://docs.openstack.org/python-novaclient/latest/reference/api/index.html)


```python
import keystoneclient.v2_0.client as ksclient
# Replace the method arguments with the ones from your local config
keystone = ksclient.Client(auth_url="http://192.168.122.100:5000/v3",
                           username="admin",
                           password="admin",
                           tenant_name="admin")
glance_service = keystone.services.create(name="glance",
                            service_type="image",
                            description="OpenStack Image Service")
auth = loader.load_from_options(auth_url="http://192.168.122.100:5000/v3", username="admin", password="admin", project_id="admin")                           
                            
```

查看日志地址为：
/var/lib/docker/volumes/kolla_logs/_data/
所有的kolla存储的日志都在这个路径下。
[root@kolla keystone]# openstack project list
+----------------------------------+---------+
| ID                               | Name    |
+----------------------------------+---------+
| 635f31a861ab4732974d7ec99e158aa8 | service |
| c04b9155ef0f4469ac55b9cd2acf2b95 | admin   |
+----------------------------------+---------+
nova中一定要使用project_id来代替name，否则会出现keystone找不到project的问题
然后添加：
/etc/kolla/config/nova/nova-compute.conf
[neutron]
user_domain_name = default
否则会出现：
Expecting to find domain in user. The server could not comply with the request since it is either malformed or otherwise incorrect. The client is assumed to be in error.

目前测试通过的代码：
```python
from openstack import connection
auth_args = {
    'auth_url': 'http://192.168.122.100:35357/v3',
    'project_name': 'admin',
    'user_domain_name': 'default',
    'project_domain_name': 'default',
    'username': 'admin',
    'password': 'admin',
}
conn = connection.Connection(**auth_args)
conn.authorize()
```
返回当前的授权码。

find / -name nova
找到nova的可执行路径为:
/var/lib/docker/overlay/0708a9436e6893d5576a69408a06620f7ec8de30792f270b90080fa7627db1b8/root/usr/bin/nova
然后执行：
/var/lib/docker/overlay/0708a9436e6893d5576a69408a06620f7ec8de30792f270b90080fa7627db1b8/root/usr/bin/nova flavor-list
获取模板结果为：
+----+-----------+-----------+------+-----------+------+-------+-------------+-----------+
| ID | Name      | Memory_MB | Disk | Ephemeral | Swap | VCPUs | RXTX_Factor | Is_Public |
+----+-----------+-----------+------+-----------+------+-------+-------------+-----------+
| 1  | m1.tiny   | 512       | 1    | 0         |      | 1     | 1.0         | True      |
| 2  | m1.small  | 2048      | 20   | 0         |      | 1     | 1.0         | True      |
| 3  | m1.medium | 4096      | 40   | 0         |      | 2     | 1.0         | True      |
| 4  | m1.large  | 8192      | 80   | 0         |      | 4     | 1.0         | True      |
| 5  | m1.xlarge | 16384     | 160  | 0         |      | 8     | 1.0         | True      |
+----+-----------+-----------+------+-----------+------+-------+-------------+-----------+
