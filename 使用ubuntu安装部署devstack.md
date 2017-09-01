# 使用ubuntu安装部署devstack

## 安装过程：
因为使用kolla安装的内容存在使用python开发不能正确授权的问题，尝试使用devstack来做非docker化的安装。
baseOS采用ubuntu16.04.3，参考文档简单的记录命令：
cd /home/ubuntu/
git clone https://github.com/openstack-dev/devstack.git
sudo devstack/tools/create-stack-user.sh
sudo chown -R stack:stack devstack
sudo chown -R stack:stack /opt/stack
sudo su stack
nano devstack/local.conf
```shell
[[local|localrc]]
ADMIN_PASSWORD=password
DATABASE_PASSWORD=password
RABBIT_PASSWORD=password
SERVICE_PASSWORD=password

# use TryStack git mirror
GIT_BASE=http://git.trystack.cn
NOVNC_REPO=http://git.trystack.cn/kanaka/noVNC.git
SPICE_REPO=http://git.trystack.cn/git/spice/spice-html5.git

# 强烈依赖于pip，如果出现版本问题，强制升级。
# 一定要设置这个。安装前依赖检查，默认是发现版本不一致的库直接中断脚本。
PIP_UPGRADE=True
```
devstack/stack.sh

然后经过漫长的等待，部署完毕会出现：
```shell
=========================
DevStack Component Timing
=========================
Total runtime    4297

run_process       25
test_with_retry    3
apt-get-update    12
pip_install      889
osc              224
wait_for_service  36
git_timed        628
dbsync           135
apt-get          968
=========================



This is your host IP address: 192.168.122.175
This is your host IPv6 address: ::1
Horizon is now available at http://192.168.122.175/dashboard
Keystone is serving at http://192.168.122.175/identity/
The default users are: admin and demo
The password: password

WARNING:
Using lib/neutron-legacy is deprecated, and it will be removed in the future


Services are running under systemd unit files.
For more information see:
https://docs.openstack.org/devstack/latest/systemd.html

DevStack Version: pike
Change:
OS Version: Ubuntu 16.04 xenial
```
然后就可以登入dashboard：
http://192.168.122.175
进行操作了。

[用DevStack安装OpenStack(单机)](http://www.ganecheng.tech/blog/53538203.html)
[OpenStack项目系列介绍（3） Devstack](http://www.chenshake.com/openstack-project-series-3-devstack/)

## CLI使用方式：
上述进入dashboard中的所有操作都可以通过命令行完成，但是最关键的是：
如果需要在cli中进行操作，需要进行环境变量设置，完成keystone的授权，否则会出现：
You are not authorized to perform the requested action: identity:XXX

为了以admin的权限进行操作，我们自己建立一个环境变量：
touch admin-openrc
nano admin-openrc
```shell
export OS_PROJECT_DOMAIN_ID=default
export OS_USER_DOMAIN_ID=default
export OS_PROJECT_NAME=admin
export OS_TENANT_NAME=admin
export OS_USERNAME=admin
export OS_PASSWORD=password
export OS_AUTH_URL=http://192.168.122.175/identity
export OS_IDENTITY_API_VERSION=3
```
source admin-openrc
然后就可以使用openstack来进行操作了。例如：
openstack endpoint list
查看当前的入口点：
```shell
+----------------------------------+-----------+--------------+----------------+---------+-----------+--------------------------------------------------+
| ID                               | Region    | Service Name | Service Type   | Enabled | Interface | URL                                              |
+----------------------------------+-----------+--------------+----------------+---------+-----------+--------------------------------------------------+
| 088257644f4d4c8a9d9559fccf072e93 | RegionOne | keystone     | identity       | True    | public    | http://192.168.122.175/identity                  |
| 2c62bfa36820445993529cf47afde5d4 | RegionOne | cinder       | volume         | True    | public    | http://192.168.122.175/volume/v1/$(project_id)s  |
| 34c5e6357db9465bba0cd0edf1b1b0d0 | RegionOne | nova         | compute        | True    | public    | http://192.168.122.175/compute/v2.1              |
| 39a82bf299de44e2b7a6510f94cbc505 | RegionOne | cinderv3     | volumev3       | True    | public    | http://192.168.122.175/volume/v3/$(project_id)s  |
| 77492a6c721d4ecea0bb5505894179b1 | RegionOne | placement    | placement      | True    | public    | http://192.168.122.175/placement                 |
| 8d61771a4fc44028a2f4998a23f35008 | RegionOne | nova_legacy  | compute_legacy | True    | public    | http://192.168.122.175/compute/v2/$(project_id)s |
| 942005836531418fb2d4eead3a74e83b | RegionOne | cinderv2     | volumev2       | True    | public    | http://192.168.122.175/volume/v2/$(project_id)s  |
| cc00d5d39f064484bfa98154d4c98a02 | RegionOne | neutron      | network        | True    | public    | http://192.168.122.175:9696/                     |
| eda99e418fc44a819103154cd255c71b | RegionOne | glance       | image          | True    | public    | http://192.168.122.175/image                     |
| f6d687d1243e497f98c28d10b7da29d3 | RegionOne | keystone     | identity       | True    | admin     | http://192.168.122.175/identity                  |
+----------------------------------+-----------+--------------+----------------+---------+-----------+--------------------------------------------------+
```
请求认证令牌：
openstack token issue
```shell
+------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Field      | Value                                                                                                                                                                                   |
+------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| expires    | 2017-08-31T03:41:20+0000                                                                                                                                                                |
| id         | gAAAAABZp3dQYVPgcyyn4YVFeVH_oPwX2BRp7QZcO7rqZycxb_yTu2TxJ9H5phL-0V7OVQ-lZgO2KncUkApoAeJp7CSr_43NLFZdHRHh7Qvli4FdRTqSn9HqW0Nw8QNKvd2DqmEpW7jwfRIxSnBBbYQwRd4QwgfQb0b0TrzPdcV3VaJ6DsKgIPI |
| project_id | db33beebbc87439597dab503cbb025e5                                                                                                                                                        |
| user_id    | ff4ce8704b3a4b87ae97cf3f30d76f6c                                                                                                                                                        |
+------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
```
查看当前的project列表：
openstack project list
```shell
+----------------------------------+--------------------+
| ID                               | Name               |
+----------------------------------+--------------------+
| 1211eea31a9d41b987b4d8fd7e8ad861 | alt_demo           |
| 186da217621b48e9887ac83f8cb7eec9 | invisible_to_admin |
| db33beebbc87439597dab503cbb025e5 | admin              |
| e39c28ff59964871a102edad8d420dce | service            |
| e6a7dc5d15e348fe88231de96872b4f1 | demo               |
+----------------------------------+--------------------+
```


参考文档：
[创建 OpenStack 客户端环境脚本](https://docs.openstack.org/liberty/zh_CN/install-guide-rdo/keystone-openrc.html)


## 使用RESTFul接口交互：

curl -v -s -X POST http://192.168.122.175/identity/auth/tokens?nocatalog   -H "Content-Type: application/json"   -d '{ "auth": { "identity": { "methods": ["password"],"password": {"user": {"domain": {"name": "'admin'"},"name": "'admin'", "password": "'password'"} } }, "scope": { "project": { "domain": { "name": "'admin'" }, "name":  "'admin'" } } }}' \
| python -m json.tool



参考文档：
[OpenStack API Documentation](https://developer.openstack.org/api-guide/quick-start/)
[OpenStack API Documentation 中文](https://developer.openstack.org/zh_CN/api-guide/quick-start/api-quick-start.html)


## 使用openstack的python接口进行编程使用：
根据[OpenStack SDK官方教程](https://developer.openstack.org/sdks/python/openstacksdk/users/index.html)进行测试代码的编写。
首先确保当前的系统中安装了openstack sdk：
pip install openstacksdk
然后导入环境变量：
source admin-openrc
编写代码测试。

参考文档：
[OpenStack Python SDK介绍](http://blog.csdn.net/xiewen99/article/details/52052727)
[OpenStack Python SDK](https://github.com/openstack/python-openstacksdk)
[Welcome to the OpenStack SDK!](https://developer.openstack.org/sdks/python/openstacksdk/)
[Getting started with the OpenStack SDK](https://developer.openstack.org/sdks/python/openstacksdk/users/index.html)
[OpenStack Pike Project User Guides](https://docs.openstack.org/pike/user/)

### 授权和登入cloud：
首先要保证当前的pytho代码可以通过认证登入openstack cloud：
```python
from openstack import connection
auth_args = {
    'auth_url': 'http://192.168.122.175/identity',
    'project_name': 'admin',
    'username': 'admin',
    'password': 'password',
}
conn = connection.Connection(**auth_args)

print "current access token:"
print(conn.authorize())

print "list servers:"
for server in conn.compute.servers():
    print(server)

print "list images:"
for image in conn.compute.images():
    print(image)

print "list flavors:"
for flavor in conn.compute.flavors():
    print(flavor)

print "list networks:"
for network in conn.network.networks():
    print(network)
```
通过上述测试，可以看到获取的内容好dashboard还有CLI中的数据一致，基本的运行环境已经可以了。

### 创建虚拟机：
使用python API创建一个VM：


参考文档：
[root/examples/compute/create.py](http://git.openstack.org/cgit/openstack/python-openstacksdk/tree/examples/compute/create.py)
