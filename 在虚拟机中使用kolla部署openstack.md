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
这部分就是在创建虚拟机了。
### （15）检查虚拟网络：
如果上一步中没有绑定浮动IP，这个查看是没有结果的。
openstack server list
回显：
```shell

```




参考文档：
[利用kolla快速搭建openstack-ocata单节点](https://www.lijiawang.org/posts/kolla-openstack-ocata.html)
本文就是按照这个文档，在KVM的centos7_x86-64上面完成了部署测试。