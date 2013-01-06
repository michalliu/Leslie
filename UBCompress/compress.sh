#!/bin/bash

# Program: compress.sh
# Author : leslie
# Email  : pythonisland at gmail dot com
# Version: 1.0
#
# History:
#        01/03/13           leslie               merge pod,pox,dod compress into one script
#        01/03/13           leslie               fix bug: program should just do compress job
#        01/03/13           leslie               add <year> check, should large then 1987
#

PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
export PATH

# load config file
source ~/.leslie

cd ~/bin

#compress data
function compress_node_pod()
{
    node=$1
    year=$2

    cd ~/bin/
    echo "$year node $node Start \$(date)" >compress_node.sh 

    for mon in $(seq -w  01 12);
    do
        echo $year$mon
        month=$year$mon
  
  	for dir in $(ls -d /UBSys/NodesPOD/$((($node-23)*2+1))/Disks/*/data_*/*_pdb*_$month/ 2> /dev/null);
	do
	    echo "gzip -f ${dir}/dat/*.ub1" >> compress_node.sh
	    echo "gzip -f ${dir}/dat/*.ub0" >> compress_node.sh
	    echo "gzip -f ${dir}/idx/*.idx" >> compress_node.sh
	done
	
	for dir in $(ls -d /UBSys/NodesPOD/$((($node-23)*2+2))/Disks/*/data_*/*_pdb*_$month/ 2> /dev/null);
	do
	    echo "gzip -f ${dir}/dat/*.ub1" >> compress_node.sh
	    echo "gzip -f ${dir}/dat/*.ub0" >> compress_node.sh
	    echo "gzip -f ${dir}/idx/*.idx" >> compress_node.sh
	done
    done
    echo "$year node $node End \$(date)" >> compress_node.sh
    bash ~/bin/compress_node.sh
    
}

function compress_node_dod()
{
    year=$1
    node=$2

    cd ~/bin/
    echo "$year node $node Start \$(date)" > compress_node.sh

    for mon in $(seq -w  01 12);
    do
	echo $year$mon
	month=$year$mon
	
	for dir in $(ls -d /UBSys/Nodes/$node/Disks/*/data_*/*_pdb*_$month/ 2> /dev/null);
	do
	    echo "gzip -f ${dir}/dat/*.ub1" >> compress_node.sh
	    echo "gzip -f ${dir}/dat/*.ub0" >> compress_node.sh
	    echo "gzip -f ${dir}/idx/*.idx" >> compress_node.sh
	done
	
	for dir in $(ls -d /UBSys/Nodes/$node/Disks/*/lewtan/lewtan_*_$month/ 2> /dev/null);
	do
	    echo "gzip -f ${dir}/dat/*.ub1" >> compress_node.sh
	    echo "gzip -f ${dir}/dat/*.ub0" >> compress_node.sh
	    echo "gzip -f ${dir}/idx/*.idx" >> compress_node.sh
	done

	for dir in $(ls -d /UBSys/Nodes/$node/Disks/*/rmbsll/rmbsll_*_$month/ 2> /dev/null);
	do
	    echo "gzip -f ${dir}/dat/*.ub1" >> compress_node.sh
	    echo "gzip -f ${dir}/dat/*.ub0" >> compress_node.sh
	    echo "gzip -f ${dir}/idx/*.idx" >> compress_node.sh
	done
    done

    echo "$year node $node End \$(date)" >> compress_node.sh

    bash ~/bin/compress_node.sh
      
}

function compress_node_has()
{
    year=$1
    node=$2

    basedir="/net/has103/mnt/z0/a0/DOD/w000"

    cd ~/bin/
    echo "$year node $node Start \$(date)" > compress_node.sh

    for mon in $(seq -w  01 12);
    do
        echo $year$mon
        month=$year$mon
	
	for dir in $(ls -d /UBSys/Nodes/$node/Disks/*/data_*/*_pdb*_$month/ 2> /dev/null);
	do
            echo "gzip -f $basedir${dir}/dat/*.ub1" >> compress_node.sh
            echo "gzip -f $basedir${dir}/dat/*.ub0" >> compress_node.sh
            echo "gzip -f $basedir${dir}/idx/*.idx" >> compress_node.sh
	done
    done
    echo "$year node $node End \$(date)" >> compress_node.sh
	
    bash ~/bin/compress_node.sh
} 



function compress_all_months_dod()
{
    year=$1
    for node in {1..108};
    do  
	echo w$node
        scp ~/bin/scripts/compress.sh  w$node:~/bin/
        ssh w$node "bash  ~/bin/compress.sh compress_node_dod  $year $node < /dev/null >> .compress_w$node 2>&1 &"
    done

}
function compress_all_months_has()
{
    year=$1
    for node in {1..108};
    do
        echo w$node
        scp ~/bin/scripts/compress.sh  w$node:~/bin/
        ssh w$node "bash  ~/bin/compress.sh compress_node_has  $year $node < /dev/null >> .compress_w$node 2>&1 &"
    done

}


function compress_all_months_pod()
{
    year=$1
    for node in {23..40};
    do  
	echo obi$node
        scp ~/bin/scripts/compress.sh  obi$node:~/bin/
        ssh obi$node "bash  ~/bin/compress.sh compress_node_pod $node $year  < /dev/null >> .compress_obi$node 2>&1 &"
    done

}
function compress_all_months_pox()
{
    year=$1
    for node in {23..40};
    do
        echo x$node
        scp ~/bin/scripts/compress.sh  x$node:~/bin/
        ssh x$node "bash  ~/bin/compress.sh compress_node_pod $node $year  < /dev/null >> .compress_x$node 2>&1 &"
    done

}

if [ $# == 0 ];
then
    echo "Usage:"
    echo "bash compress.sh compress_all_months_pod <year>"
    echo "bash compress.sh compress_all_months_pox <year>"
    echo
    echo "bash compress.sh compress_all_months_dod <year>"
    echo "bash compress.sh compress_all_months_has <year>"
    exit 1
fi

[ "$1" == "compress_all_months_pod" ] || [ "$1" == "compress_all_months_pox" ] || 
	[ "$1" == "compress_all_months_dod" ] || [ "$1" == "compress_all_months_has" ] || exit -1

[ $2 -ge 1987 ] || exit -1

eval $*
