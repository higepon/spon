#!/bin/sh

LN=/bin/ln
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

$SED -e "18 c (define download-uri \"$SPON_URI\")" \
     -e "19 c (define base-path \"$SPON_HOME\")" \
     -e "20 c (define command-path \"$SPON_COMMAND\")" \
     -e "21 c (define library-path \"$SPON_LIB\")" \
     -e "22 c (define document-path \"$SPON_DOC\")" \
     -e "23 c (define source-path \"$SPON_SRC\")" \
     -e "24 c (define share-path \"$SPON_SHARE\")" \
     -e "25 c (define temporary-path \"$SPON_TMP\")" \
     config.tmpl.sls > config.sls

echo -e "#!/bin/sh\nmosh $SPON_HOME/spon.ss \$*" > spon.mosh.sh
echo -e "#!/bin/sh\nypsilon $SPON_HOME/spon.ss \$*" > spon.ypsilon.sh

$INSTALL -v -m 755 -d $SPON_HOME
$INSTALL -v -m 755 -d $SPON_LIB
$INSTALL -v -m 755 -d $SPON_LIB/spon
$INSTALL -v -m 755 -d $SPON_DOC
$INSTALL -v -m 755 -d $SPON_SRC

for f in spon.mosh.sh spon.ypsilon.sh; do
    $INSTALL -v -m 755 $f $SPON_HOME
done

for f in spon.ss setup.mosh.ss setup.ypsilon.ss; do
    $INSTALL -v -m 644 $f $SPON_HOME
done

for f in sponrc.sample; do
    $INSTALL -v -m 644 $f $SPON_DOC
done

for f in compat.sls compat.mosh.sls compat.ypsilon.sls config.sls tools.sls; do
    $INSTALL -v -m 644 $f $SPON_LIB/spon
done

CWD=`pwd`
cd $SPON_LIB
$SCHEME_SCRIPT $SPON_HOME/setup.$SCHEME_SCRIPT.ss
cd $CWD
$LN -sf $SPON_HOME/spon.$SCHEME_SCRIPT.sh $SPON_COMMAND
