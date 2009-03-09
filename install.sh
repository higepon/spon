#!/bin/bash

SPON_COMMAND=/usr/local/bin/spon
SPON_HOME=/usr/local/share/spon

SCHEME=mosh
#SCHEME=ypsilon

INSTALL=/usr/bin/install

$INSTALL -v -m 755 -d $SPON_HOME
$INSTALL -v -m 755 -d $SPON_HOME/doc
$INSTALL -v -m 755 -d $SPON_HOME/spon

$INSTALL -v -m 644 spon.ss $SPON_HOME
$INSTALL -v -m 644 sponrc.sample $SPON_HOME/doc

$INSTALL -v -m 644 base.sls $SPON_HOME/spon
$INSTALL -v -m 644 compat.sls $SPON_HOME/spon
$INSTALL -v -m 644 compat.mosh.sls $SPON_HOME/spon
$INSTALL -v -m 644 compat.ypsilon.sls $SPON_HOME/spon
$INSTALL -v -m 644 tools.sls $SPON_HOME/spon

case "$SCHEME" in
  'mosh')
    echo "#!/bin/bash" > spon.sh
    echo "mosh --loadpath=$SPON_HOME $SPON_HOME/spon.ss" >> spon.sh
    $INSTALL -v -D -m 755 spon.sh $SPON_COMMAND
    ;;
  'ypsilon')
    echo "#!/bin/bash" > spon.sh
    echo "ypsilon --sitelib=$SPON_HOME $SPON_HOME/spon.ss" >> spon.sh
    $INSTALL -v -D -m 755 spon.sh $SPON_COMMAND
    ;;
  *)
    echo "ERROR!"
    ;;
esac
