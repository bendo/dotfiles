#!/bin/bash

store=".gram-de"
lines=()
todo_lessons=()

declare -A levels
levels[T]=TODO
levels[A]=APPRENTICE
levels[G]=GURU
levels[M]=MASTER
levels[E]=ENLIGHTENED
levels[B]=BURNED

red="\033[31m"
green="\033[32m"
orange="\033[33m"
blue="\033[34m"
pink="\033[35m"
dark="\033[30m"
bold="\033[1m"
none="\033[0m"

readfile()
{
    if [[ ! -f ~/$store ]]; then
        > ~/$store
    fi

    sed -i '/^$/d' ~/$store

    while read line; do
        lines+=("$line")
    done < ~/$store
}

add_lesson()
{
    readfile
    new_lesson=$1
    if [[ $new_lesson -le 0 || $new_lesson -gt 80 ]]; then
        echo "Lesson has to be in interval 1 to 80."
        exit 1;
    fi
    printf -v ll "%02d" $new_lesson
    echo "Adding lesson $ll"

    isnewlesson=true
    for i in "${lines[@]}"; do
        lesson=($(echo "$i" | cut -d \| -f 1))
        if [[ $ll -eq $lesson ]]; then
            level=($(echo "$i" | cut -d \| -f 2))
            updated="$ll | $(get_new_level $level) | $(get_new_date $level)"
            sed -i '/'"${i}"'/c\'"$updated" ~/$store
            isnewlesson=false
        fi
    done
    if [[ $isnewlesson == true ]]; then
        newlesson="$ll | $(get_new_level "X") | $(get_new_date "X")"
        echo $newlesson >> ~/$store
    fi
    sort -o ~/$store{,}
}

get_new_date()
{
    level=$1
    today=$(get_date)
    case $level in
        X) new_date=$(date '+%Y-%m-%d' -d "$today+4 days");;
        A) new_date=$(date '+%Y-%m-%d' -d "$today+9 days");;
        G) new_date=$(date '+%Y-%m-%d' -d "$today+13 days");;
        M) new_date=$(date '+%Y-%m-%d' -d "$today+21 days");;
        E) new_date=$(date '+%Y-%m-%d' -d "$today+34 days");;
        B) new_date=$(date '+%Y-%m-%d' -d "$today+55 days");;
    esac
    echo $new_date
}

get_new_level()
{
    level=$1
    case $level in
        X) new_level=A;;
        A) new_level=G;;
        G) new_level=M;;
        M) new_level=E;;
        E) new_level=B;;
        B) new_level=B;;
    esac
    echo $new_level
}

overview()
{
    readfile
    lessons=({1..80})

    declare -A started

    read -r a g m e b <<< $(echo 0 0 0 0 0)

    for i in "${lines[@]}"; do
        lesson=($(echo "$i" | cut -d \| -f 1))
        level=($(echo "$i" | cut -d \| -f 2))
        started[$lesson]=$(put_color $lesson $level)

        case $level in
            A)((a+=1));;
            G)((g+=1));;
            M)((m+=1));;
            E)((e+=1));;
            B)((b+=1));;
        esac
    done

    t=$((80-($a+$g+$m+$e+$b)))

    printf "\n"$bold"Lessons overview:"$none"\n\n"
    for l in ${lessons[@]}; do
        notstarted=true
        printf -v ll "%02d" $l
        for i in "${!started[@]}"; do
            if [[ $i == $ll ]]; then
                printf " ${started[$i]} "
                notstarted=false
            fi
        done
        if [[ $notstarted == true ]]; then
            printf " $ll "
        fi
        if (( $l % 20 == 0)); then
            printf "\n"
        fi
    done
    printf "\n"
    printf ${levels[T]}"($t)   "
    printf $pink${levels[A]}$none"($a)   "
    printf $orange${levels[G]}$none"($g)   "
    printf $green${levels[M]}$none"($m)   "
    printf $blue${levels[E]}$none"($e)   "
    printf $dark${levels[B]}$none"($b)\n\n"
}

version()
{
cat <<- EOF
gram 0.8
Copyright (C) 2019 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by Zdeno Osina.
EOF
}

get_date()
{
    date --utc +"%Y-%m-%d"
}

todo()
{
    readfile
    if [[ ! -s ~/$store ]]; then
        exit 0
    fi
    for i in "${lines[@]}"
    do
        # echo $i
        if [[ $(echo "$i" | cut -d \| -f 3) < $(get_date) ]]; then
            lesson=($(echo "$i" | cut -d \| -f 1))
            level=($(echo "$i" | cut -d \| -f 2))
            if [[ $level != B ]]; then
                todo_lessons+=($(put_color $lesson $level))
            fi
        fi
    done
}

print_todo()
{
    printf $bold"TODO:"$none" "
    todo
    echo "${todo_lessons[@]}"
}

count()
{
    todo
    echo "${#todo_lessons[@]}"
}

put_color()
{
    lesson=$1
    level=$2

    case $level in
        A)color=$pink;;
        G)color=$orange;;
        M)color=$green;;
        E)color=$blue;;
        B)color=$dark;;
        *)color=$none;;
    esac
    echo -e $bold$color"$1"$none
}

usage()
{
cat <<- EOF
Gram - Grammatik aktiv SRS system
Description: Gram is spaced repetition learning system tool to help learn book
    (Grammatik aktiv - Cornelsen - daf) Book has 80 lessons.
Usage: gram
    -o Show overview
    -t Show lessons which should be repeated
    -c Show count of lessons which should be repeated
    -a Add new lesson
    -h Show the help
    -v Get the tool version
Examples:
    gram -o
    gram -t
    gram -a 20
EOF
}

while getopts :a:otchv opt; do
    case "$opt" in
        a) add_lesson $OPTARG;;
        o) overview;;
        h) usage;;
        v) version;;
        t) print_todo;;
        c) count;;
        \?) echo "Invalid option: -$OPTARG" >&2 ; exit 1;;
        :) echo "Option -$OPTARG requires an argument." >&2 ; exit 1;;
    esac
done

if [[ $# == 0 ]]; then
    print_todo
elif [[ $1 == "help" ]]; then
    usage
elif [[ $1 == "version" ]]; then
    version
fi
