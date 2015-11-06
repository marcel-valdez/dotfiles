#!/bin/bash

function ant-start-all() {
  ant start-k start-rdbms start-ss start-appserver
}

export -f ant-start-all

function ant-stop-all() {
  ant stop-appserver stop-ss stop-k stop-rdbms
}

export -f ant-stop-all


# quick checkout
function git-checkout-quick ()
{
 touch artifacts/*.touch
 git checkout $1
 touch artifacts/*.touch
}

# b) function cd_func
# This function defines a 'cd' replacement function capable of keeping,
# displaying and accessing history of visited directories, up to 10 entries.
# To use it, uncomment it, source this file and try 'cd --'.
# acd_func 1.0.5, 10-nov-2004
# Petar Marinov, http:/geocities.com/h2428, this is public domain
 function cd_func ()
 {
   local x2 the_new_dir adir index
   local -i cnt

   if [[ $1 ==  "--" ]]; then
     dirs -v
     return 0
   fi

   the_new_dir=$1
   [[ -z $1 ]] && the_new_dir=$HOME

   if [[ ${the_new_dir:0:1} == '-' ]]; then
     #
     # Extract dir N from dirs
     index=${the_new_dir:1}
     [[ -z $index ]] && index=1
     adir=$(dirs +$index)
     [[ -z $adir ]] && return 1
     the_new_dir=$adir
   fi

    #
   # '~' has to be substituted by ${HOME}
   [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

   #
   # Now change to the new dir and add to the top of the stack
   pushd "${the_new_dir}" > /dev/null
   [[ $? -ne 0 ]] && return 1
   the_new_dir=$(pwd)

   #
   # Trim down everything beyond 11th entry
   popd -n +11 2>/dev/null 1>/dev/null

   #
   # Remove any other occurence of this dir, skipping the top of the stack
   for ((cnt=1; cnt <= 10; cnt++)); do
     x2=$(dirs +${cnt} 2>/dev/null)
     [[ $? -ne 0 ]] && return 0
     [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
     if [[ "${x2}" == "${the_new_dir}" ]]; then
       popd -n +$cnt 2>/dev/null 1>/dev/null
       cnt=cnt-1
     fi
   done

   return 0
 }

 # Appian development functions
 function edit_bashrc_func() {
  nano $HOME/.bashrc
 }

 function ae() {
  if [ "$(expr substr $(uname) 1 6)" == "CYGWIN" ]; then
    cd $HOME/repositories/ae$1
  else
    cd $HOME/repositories/ae$1
  fi

  tmux-title-git-branch
 }

 function tmux-title-git-branch() {
   tmux-title $(git-current-branch | cut -c1-15)
 }

 function tmux-title() {
    if in_tmux; then
      tmux rename-window $1;
    fi
 }

 function in_tmux() {
   if [ "$TERM" = "screen" ] && [ -n "$TMUX" ]; then
     return 0;
   else
     return 1;
   fi
 }

 function in_git_repo() {
   git rev-parse > /dev/null 2>&1 && echo true;
   return 0;
 }

 function merge-with-appian() {
  if [ -z "$1" ]; then
    echo "This function creates a new branch off of your current branch, and then"
    echo "merges that branch with a appian/xxx branch, where xxx is the parameter"
    echo "this function accepts."
    echo ""
    echo "Error: No appian branch to merge with was supplied, you must supply one."
    echo "Example: "
    echo "merge-with-appian 7.4.0.0-maint"

    return 1;
  fi

  is_git_repo=`git rev-parse > /dev/null 2>&1 && echo true`
  if [ "$is_git_repo" != "true" ]; then
    echo "You are not inside a git repository, cannot proceed."
    return 1;
  fi

  appian_exists=`git ls-remote appian > /dev/null 2>&1 && echo true`
  if [ "$appian_exists" != "true" ]; then
    echo "The remote repository alias 'appian' does not exist."
    return 1;
  fi

  remote_branch=`git ls-remote appian | grep "$1\$"`
  if [ -z "$remote_branch" ]; then
    echo "The branch $1 does not exist in remote repository 'appian'"
    return 1;
  fi

  current_branch=`git rev-parse --abbrev-ref HEAD`
  new_branch="$current_branch-$1"
  echo "Merging branch $current_branch with: appian/$1 on new branch $new_branch"
  git branch $new_branch && git checkout $new_branch && git merge appian/$1

  return 0;
 }

 # list files in git status that are currently staged, not staged and untracked, but removes any prefixes, only gives the list of files.
 function list-files() {
   sed_expr='s/#\t$1\:[ ]*//'
   echo git status | grep "$1" | sed $sed_expr

   return 0
 }

 function last() {
   tailno=$1
   regex=$2
   if [ "$regex" = "" ]; then
     regex=$1
     tailno="-1"
   fi

   result=`history | sed 's/^\s*[0-9]*\s*//' | grep -v '^history ' | grep -v '^last ' | grep $regex | tail $tailno | head -1`
   echo $result
 }

 function ..() {
   cd ..
 }
