#!/bin/sh
INSTALL=/usr/bin/install

SCHEME_SCRIPT=${1:-mosh}
PREFIX=${2:-/usr/local}

SPON_COMMAND=$PREFIX/bin/spon
SPON_HOME=$PREFIX/share/spon

$INSTALL -v -m 755 -d $SPON_HOME
$INSTALL -v -m 755 -d $SPON_HOME/doc
$INSTALL -v -m 755 -d $SPON_HOME/spon

echo "#!/bin/sh" > spon.sh

case "$SCHEME_SCRIPT" in
  'mosh')
    echo "mosh --loadpath=$SPON_HOME $SPON_HOME/spon.ss \$*" >> spon.sh
    ;;
  'ypsilon')
    echo "ypsilon --sitelib=$SPON_HOME $SPON_HOME/spon.ss \$*" >> spon.sh
    ;;
  *)
    echo "ERROR!"
    exit 1
    ;;
esac

$INSTALL -v -m 755 spon.sh $SPON_COMMAND

for f in spon.ss; do
    $INSTALL -v -m 644 $f $SPON_HOME
done

for f in sponrc.sample; do
    $INSTALL -v -m 644 $f $SPON_HOME/doc
done

for f in base.sls compat.sls compat.mosh.sls compat.ypsilon.sls tools.sls; do
    $INSTALL -v -m 644 $f $SPON_HOME/spon
done
