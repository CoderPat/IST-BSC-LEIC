#!/bin/sh

repositories=(AMS-16-17 SD-16-17 BD-16-17 IA-16-17 RC-16-17 IPM-15-16 ASA-15-16)

shopt -s extglob
for repo in ${repositories[@]}
do
	git remote add -f ${repo} git@github.com:CoderPat/${repo}.git
	git merge --allow-unrelated-histories ${repo}/master -m "merge ${repo} into the main one"
	mkdir ${repo} 
	mv !($(IFS="|" ; echo "${repositories[*]}")|merging-script.sh) ${repo}/ 
	git add -A .
	git commit -m "move subrepository ${repo} to its own folder"
done
