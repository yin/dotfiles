#!/bin/bash

source_dir=$HOME/install/eclipse
deploy_dir=$HOME/.apps/eclipse
symlink_dir=$HOME/.bin

if [ ! -d $source_dir ] || ! ls $source_dir/eclipse* > /dev/null; then
	echo "Place eclipse install files into: $source_dir" >&2
	echo mkdir -p $source_dir
	mkdir -p $source_dir
	exit 1
fi

echo mkdir -p $deploy_dir
mkdir -p $deploy_dir

for p in $(ls $source_dir/eclipse*); do
	f=${p##$(dirname $p)}
	f=${f##/}
	d=${f%.tar.gz}
	if [ "$d" != "$f" ]; then
		echo $d
		dir=$deploy_dir/$d
		bin=$dir/eclipse/eclipse
		if [ -d $dir ]; then
			echo "  ... Pressent"
		else
			echo "  Installing..."
			echo mkdir -p $dir
			echo tar xzf $p -C $dir
			mkdir -p $dir
			tar xzf $p -C $dir
		fi
		if [ -f $bin ] && [ ! -x $bin ]; then
			echo chmod go-rwx $dir/eclipse/eclipse
			echo chmod u+x $dir/eclipse/eclipse
			chmod go-rwx $dir/eclipse/eclipse
			chmod u+x $dir/eclipse/eclipse
		else
			echo "  ... permissions OK"
		fi
		if [ "$(find $symlink_dir/* -maxdepth 1 -lname '*'$bin'*' | wc -l)" -eq "0" ]; then
			echo ln -s $dir/eclipse/eclipse $symlink_dir/$d
			ln -s $dir/eclipse/eclipse $symlink_dir/$d
		else
			echo "  ... symlink present"
		fi
	else
		echo "Archive format unknown: $i" >&2
	fi
done

