# Hellgate

Core logic service for payment states processing.

## Building

We widelly use Thrift to define RPC protocols.
So it needs to have [our Thrift compiler](https://github.com/rbkmoney/thrift) in PATH to build this service.
The recommended way to achieve this is by using our [build image](https://github.com/rbkmoney/image-build-erlang).

We are still experimenting on opening our build infrastructure so that you can use the explicit public registry setting for now.
You can adjust this parameter by exporting the environment variable `REGISTRY`.

### Сheatsheet

To build the service image without access to the internal RBK.money registry:

```shell
make submodules && REGISTRY=ghcr.io make wc_release build_image
```

To compile:

```shell
make submodules && REGISTRY=ghcr.io make wc_compile
```

To run the service tests (you need either to have access to the internal RBK.money registry or to modify `docker-compose.sh`):

```shell
make wdeps_test
```
