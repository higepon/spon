#!/bin/sh

SED=/bin/sed
INSTALL=/usr/bin/install

SCHEME_SCRIPT=${1:-mosh}
PREFIX=${2:-/usr/local}

SPON_URI=http://scheme-users.jp/spon
SPON_HOME=$PREFIX/share/spon
SPON_COMMAND=$PREFIX/bin/spon
SPON_LIB=$SPON_HOME/lib
SPON_DOC=$SPON_HOME/doc
SPON_SRC=$SPON_HOME/src
SPON_SHARE=$SPON_HOME/share
SPON_TMP=/tmp

$SED -e "14 c (define *spon-uri* \"$SPON_URI\")" \
     -e "15 c (define *spon-home* \"$SPON_HOME\")" \
     -e "16 c (define *command-path* \"$SPON_COMMAND\")" \
     -e "17 c (define *library-path* \"$SPON_LIB\")" \
     -e "18 c (define *document-path* \"$SPON_DOC\")" \
     -e "19 c (define *source-path* \"$SPON_SRC\")" \
     -e "20 c (define *share-path* \"$SPON_SHARE\")" \
     -e "21 c (define *temporary-path* \"$SPON_TMP\")" \
     config.tmpl.sls > config.sls

echo "#!/bin/sh" > spon.sh

case "$SCHEME_SCRIPT" in
  'mosh')
    echo "mosh $SPON_HOME/spon.ss \$*" >> spon.sh
    ;;
  'ypsilon')
    echo "ypsilon $SPON_HOME/spon.ss \$*" >> spon.sh
    ;;
  *)
    echo "ERROR!"
    exit 1
    ;;
esac

$INSTALL -v -m 755 -d $SPON_HOME
$INSTALL -v -m 755 -d $SPON_LIB
$INSTALL -v -m 755 -d $SPON_LIB/spon
$INSTALL -v -m 755 -d $SPON_DOC
$INSTALL -v -m 755 -d $SPON_SRC

$INSTALL -v -m 755 spon.sh $SPON_COMMAND

for f in spon.ss install.mosh.ss; do
    $INSTALL -v -m 644 $f $SPON_HOME
done

for f in sponrc.sample; do
    $INSTALL -v -m 644 $f $SPON_DOC
done

for f in base.sls compat.sls compat.mosh.sls compat.ypsilon.sls config.sls tools.sls; do
    $INSTALL -v -m 644 $f $SPON_LIB/spon
done

$SCHEME_SCRIPT $SPON_HOME/install.$SCHEME_SCRIPT.ss $SPON_LIB
