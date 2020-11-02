# Helper script to setup LLVM on Mac OS

if [[ "$OSTYPE" == "darwin"* ]]; then
	LLVM_CACHE_PATH=/tmp/llvm-path

	if [ ! -f "$LLVM_PATH" ]; then
		echo `(/usr/local/bin/brew --prefix llvm)` > $LLVM_CACHE_PATH
	fi

	LLVM_PATH=`cat $LLVM_CACHE_PATH`

	# Put brew llvm early in the path before Mac OS' one
	export PATH=$LLVM_PATH/bin:$PATH

	# Compilation flags
	CC=clang
	CXX=clang++

	AR=llvm-ar
	LD=llvm-ld

	LDFLAGS="-L$LLVM_PATH/lib -Wl,-rpath,$LLVM_PATH/lib"
	CPPFLAGS="-I$LLVM_PATH/include -I$LLVM_PATH/include/c++/v1/"

	alias lldb='PATH="/usr/bin:$PATH" $LLVM_PATH/bin/lldb'
fi
