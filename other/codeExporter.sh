#!/bin/bash

cd ../out

files=`find tk -type f` 

for file in $files; do
	echo "$file"
	echo "\`\`\`scala"
	cat "$file"
	echo "\`\`\`"
	echo
done
