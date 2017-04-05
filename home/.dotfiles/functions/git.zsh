#!/bin/bash

function _gfeat () {
  local curcontext="$curcontext" state line
  typeset -A opt_args

  _arguments \
    '1: :->method'\
    '*: :->city'

  case $state in
    method)
      _arguments '1:Methods:(feature fix hotfix chore close)'
      ;;
  esac
}

function gfeat () {
  if [[ $1 == "close" ]]; then
    if [[ $2 == "-" || -z $2 ]]; then
      branchToBeDeleted="$(git rev-parse --abbrev-ref -q HEAD)"
      gb -
      git merge "${@:3}" -
      gbda "$branchToBeDeleted"
    fi
  else
    gbc $1/$2
  fi
}

compdef _gfeat gfeat

function delete_merged_git_branches() {
  # Remove already deleted branches on repo host
  git fetch --prune;

  # Check if arguments were passed
  if [[ -z "$@" ]]; then
    echo "delete_merged_git_branches BRANCH_NAME"
    exit 0
  fi

  # Delete branches that are merged into the branch that was passed as an argument
  # https://gist.github.com/schacon/942899#gistcomment-1940407
  git branch -r --merged | grep rigin | grep -v '>' | grep -v "$1" | xargs -L1 | awk '{sub(/origin\//,"");print}' | xargs git branch -D
  git branch -r --merged | grep rigin | grep -v '>' | grep -v "$1" | xargs -L1 | awk '{sub(/origin\//,"");print}' | xargs git push origin --delete
}

# Quick first initialize, adding all files & folders and making the first commit.
function gini {
  git init
  git add .
  if [[ "$#" == 0 ]]; then
    git commit -m "First initialize"
  else
    git commit -m "$1"
  fi
}

function git_add() {
  if [[ -z "$@" ]]; then
    git add --all :/
  else
    git add $@
  fi
}

# Opens the github page for the current git repository in your browser
# git@github.com:jasonneylon/dotfiles.git
# https://github.com/jasonneylon/dotfiles/
function gh() {
  giturl=$(git config --get remote.origin.url)
  if [ "$giturl" == "" ]
    then
     echo "Not a git repository or no remote.origin.url set"
     exit 1;
  fi

  giturl=${giturl/git\@github\.com\:/https://github.com/}
  giturl=${giturl/\.git/\/tree/}
  branch="$(git symbolic-ref HEAD 2>/dev/null)" ||
  branch="(unnamed branch)"     # detached HEAD
  branch=${branch##refs/heads/}
  giturl=$giturl/tree/$branch
  open "$giturl"
}

# take this repo and copy it to somewhere else minus the .git stuff.
function gitexport() {
  mkdir -p "$1"
  git archive master | tar -x -C "$1"
}

# Add all files and ammend to last commit
function gam () {
  CURRENT_COMMIT_SHA="$(git rev-parse HEAD)"

  # Failsafe when the last commit was already pushed
  if [[ $(git branch -r --contains $CURRENT_COMMIT_SHA) ]]; then
    SHORT_HASH=$(git rev-parse --short $CURRENT_COMMIT_SHA)
    _error_msg "The last commit $SHORT_HASH was already pushed!"
    return 1
  fi

  if [[ -z "$1" ]]; then
    # When no argument is passed
    # Amend everything without a message change
    git add . &>/dev/null
  else
    # When an argument is passed
    # Check if they are files or directories
    # Otherwise use them as a commit message
    for arg in "$@"; do
      if [[ -f "$arg" ]]; then
        echo "Adding file: $@"
        git add "$arg" &>/dev/null
      else
        msg="-m"
        msg+="$arg"
        echo -e "Changing the commit message to:\n$arg"
      fi
    done
  fi
  git commit --amend --no-edit "$msg"
}

# [bb]
# git branch that creates a branch if it doesn't exist in the local
# repo but does in the origin branch
# note: gb is set by default for some reason
function gitswitchbranch () {
  # Show branch list if there were no input parameters
  if [[ -z "$@" ]]; then
    git branch
    return
  fi

  local git_branch="$1"

  # Switch to local branch if one exists
  # Or switch to origin branch if one exists and create a local branch
  # Or switch to the last branch if input is "-"
  if [[ "$git_branch" == "-" ]]; then
    git checkout "-"
  elif [[ -n $(git show-ref "refs/heads/$git_branch") ]]; then
    git checkout "$git_branch"
  elif [[ -n $(git show-ref "origin/$git_branch") ]]; then
    git checkout -b "$git_branch" "origin/$git_branch"
  elif revlist="$(git rev-list HEAD.."$git_branch" 2>/dev/null)" && [ -z "$revlist" ]; then
    git checkout "$git_branch"
  elif [[ "$git_branch" == "dev" ]]; then
    gbc dev
  else
    echo "No such branch $git_branch"
  fi
}

function sap () {
  stashes="$(git stash list 2>/dev/null)"
  if [[ ! -z "$stashes" ]]; then
    echo "Applying & Dropping Stash!"
    git stash apply -q
    git stash drop -q
    return true
  else
    echo "Stashed modified files!"
    git stash -q
  fi
}

# Check out the latest branch
function glat () {
  # Return branches sorted by commitdate and reduce the count to one
  # Samples local and remote branches
  # http://stackoverflow.com/a/5972362
  branch="$(git for-each-ref --count=1 --sort=-committerdate refs/heads/ refs/remotes/ --format='%(refname:short)')"
  gitswitchbranch "${branch##origin/}" # Remove the origin from the branch name
}

# List worktree skipping files
function skipped () {
  git ls-files -v "$(git rev-parse --show-toplevel)" | grep "^[S]"
}

# List unchanged assumed files
function assumed () {
  git ls-files -v "$(git rev-parse --show-toplevel)" | grep "^[a-z]"
}

# Assume a file
function skip () {
  git update-index --skip-worktree "$@"
}

function unskip () {
  git update-index --no-skip-worktree "$@"
}

function ass () {
  assumefiles_from_file true
}

function unass () {
  assumefiles_from_file false
}

function unassume () {
    git update-index --no-assume-unchanged "$@"
}

function clonecd {
    url=$1;
    # If there is no user input name the repo folder after the repo name
    if [[ -z "$2" ]]; then
      reponame=$(echo "$url" | awk -F/ '{print $NF}' | sed -e 's/.git$//');
    else
      reponame="$2"
    fi
    git clone --recursive "$url" "$reponame";
    cd "$reponame";
}
function groot () {
  cd "$(git rev-parse --show-toplevel)"
}

function gbd () {
  if [[ "$1" == "-" ]]; then
    git branch -D @{-1}
  else
    git branch -D "$@"
  fi
}

# Either push current branch or given branch
function git_push () {
  if [[ -z "$@" ]]; then
    git push -u origin HEAD --tags
  elif [[ "$@" == "--all" || "$@" == "-a" ]]; then
    git push -u --all origin
  else
    git push -u origin "$@" --tags
  fi
}

function _git_branch_delete_and_push () {
  branches=`git branch -a --no-color | sed "s/\(remotes\/[^\/]*\/\)//g" | sed "/HEAD.*/d" | sed "/\*.*/d" | uniq -u`
  compadd `echo $branches | sed "s/ //g"`
}

# [git_branch_delete_and_push]:
# Deletes remote and local git branch
# @1: [branch]
git_branch_delete_and_push () {
  if [[ -z "$@" ]]; then
    echo "tip [gbda]:"
    echo "Deletes remote and local git branch"
    echo "@1: {branchname}"
  fi
  for var in "$@"; do
    git branch -D "$var"
    # Delete remote branch if one exists
    if [[ $(git branch -a | egrep "remotes/origin/$var") ]]; then
      git push origin ":$var"
    fi
  done
}

compdef _git_branch_delete_and_push git_branch_delete_and_push

# Change the current origin remote url
function git-change-remote () {
 git remote remove origin
 git remote add origin "$@"
}

# Quickly go to various branches
function master () {
  gb master
}
function dev () {
  gb dev
}

# Bitbucket Sevices
# Has to have bitbucket CLI installed

# Creates a bitbucket repo
# bbcreate Repository-Name
# $1: The name of the Repo, that you want to create
function bbcreate () {
  # Check if there is a name given for the repo
  if [[ -z $1 ]]; then
    echo "Please insert a name for the repository"
    exit
  fi
  bb create_from_local --username artish --protocol ssh artish "$1"
}

# Set the current or defined branch to the upstream
function gset () {
  if [[ -z $1 ]]; then
    branch=$(git rev-parse --abbrev-ref HEAD)
  else
    branch=$1
  fi
  git branch --set-upstream-to=origin/"$branch" "$branch"
}

# Reset and clean everything
function grha () {
  git reset --hard
  git clean -f -d
}

function gcopy () {

  if [[ -z $@ ]]; then
    echo "gitcopy SHA-FROM SHA-TIL Destination"
    echo "Copy the changed files between two commits"
    return
  fi

  rsync -R "$(git diff --name-only "$1" "$2")" "$3"
}

function gignore () {

  if [[ -z $@ ]]; then
    echo "Remove a file from a repository and write it to the .gitignore"
    return
  fi

  git rm --cached "$1"
  groot
  echo -en "\n$1" >> .gitignore
  cd -

}

function gup () {
  git checkout "HEAD^"
}

function gashd () {
  git stash apply
  git stash drop
}

# Remove and ignore a file by it's file type (or just the specific file)
function g_ignore_and_remove () {
  if [[ -z "$@" ]]; then
    echo "Remove and ignore a file by it's file type (or just the specific file)"
    echo "git_remove_and_ignore \"file\""
    return
  fi
  echo "$1" >> .gitignore
  git rm --cached "$1"
}

function git_log_full_screen_no_pager() {
  clear
  git --no-pager log --oneline -n $(($(tput lines)/2)) --graph --all --decorate
}
function igl() {
  groot # cd into git root dir
  git_log_full_screen_no_pager
  fswatch .git/refs/heads | (while read; do git_log_full_screen_no_pager; done)
  cd -
}

# Rebase from current branch
function grb () {
  dst_branch="$(git rev-parse --abbrev-ref HEAD)"
  git checkout "$1"
  git rebase "$dst_branch"
  git checkout "$dst_branch"
}

function git_check_staged () {

  # Get th
  staged_files="$(git diff --staged --name-only)"

  if [[ ! -z "$staged_files" ]] || [[ ! -z "$2" ]]; then
    git commit -m "$@"
  else
    git commit -am "$@"
  fi

}
