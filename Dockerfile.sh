#!/bin/bash
cat <<EOF
FROM $BASE_IMAGE
MAINTAINER Andrey Mayorov <a.mayorov@rbkmoney.com>
COPY ./_build/prod/rel/hellgate /opt/hellgate
CMD /opt/hellgate/bin/hellgate foreground
LABEL base_image_tag=$BASE_IMAGE_TAG
LABEL build_image_tag=$BUILD_IMAGE_TAG
# A bit of magic to get a proper branch name
# even when the HEAD is detached (Hey Jenkins!
# BRANCH_NAME is available in Jenkins env).
LABEL branch=$( \
  if [ "HEAD" != $(git rev-parse --abbrev-ref HEAD) ]; then \
    echo $(git rev-parse --abbrev-ref HEAD); \
  elif [ -n "$BRANCH_NAME" ]; then \
    echo $BRANCH_NAME; \
  else \
    echo $(git name-rev --name-only HEAD); \
  fi)
LABEL commit=$(git rev-parse HEAD)
LABEL commit_number=$(git rev-list --count HEAD)
WORKDIR /opt/hellgate
EOF

