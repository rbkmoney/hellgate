FROM dr.rbkmoney.com/rbkmoney/service_erlang:latest
MAINTAINER Andrey Mayorov <a.mayorov@rbkmoney.com>
COPY ./_build/prod/rel/hellgate /opt/hellgate
CMD /opt/hellgate/bin/hellgate foreground
LABEL service_version="semver"
WORKDIR /opt/hellgate

