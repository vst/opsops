#!/usr/bin/env bash

## Declare defaults:
_def_name="haskell-template-hebele"
_def_title="Haskell Project Template"
_def_github="vst/haskell-template-hebele"
_def_author="Vehbi Sinan Tunalioglu"
_def_maintainer="vst@vsthost.com"
_def_copyright="Copyright (c) 2024 Vehbi Sinan Tunalioglu"

## Initialize variables:
_var_name="${_def_name}"
_var_title="${_def_title}"
_var_github="${_def_github}"
_var_author="${_def_author}"
_var_maintainer="${_def_maintainer}"
_var_copyright="${_def_copyright}"

## Let's start:
cat <<EOF
You are about to configure the Haskell Template Project. I will ask
you some questions. Then, I will configure your project according to
your answers.

EOF

read -p "Do you want to proceed? (y/N) " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    >&2 echo "Exiting..."
fi
echo

echo
echo "1/6. Project Name: The project name must be a valid Haskell package name. See <https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-name>."
echo
read -p "Enter the project name [${_def_name}]: " _var_name
_var_name="${_var_name:-"${_def_name}"}"
echo

echo
echo "2/6. Project Title: The project title appearing on the README header, CLI header etc..."
echo
read -p "Enter the project title [${_def_title}]: " _var_title
_var_title="${_var_title:-"${_def_title}"}"
echo

echo
echo "3/6. GitHub Repository: This must be in the form of <repository-owner>/<repository-name>."
echo
read -p "Enter the GitHub repository [${_def_github}]: " _var_github
_var_github="${_var_github:-"${_def_github}"}"
echo

echo
echo "4/6. Author: Who is the author? See <https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-author>"
echo
read -p "Enter the author [${_def_author}]: " _var_author
_var_author="${_var_author:-"${_def_author}"}"
echo

echo
echo "5/6. Maintainer: Who is the maintainer? See <https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-maintainer>"
echo
read -p "Enter the maintainer [${_def_maintainer}]: " _var_maintainer
_var_maintainer="${_var_maintainer:-"${_def_maintainer}"}"
echo

echo
echo "6/6. Copyright: What is the copyright notice? See <https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-copyright>"
echo
read -p "Enter the copyright [${_def_copyright}]: " _var_copyright
_var_copyright="${_var_copyright:-"${_def_copyright}"}"
echo

## Confirmation:
echo
echo "Here is what you want:"
echo "======================"
echo
cat <<EOF
1. Project Name      : ${_var_name}
2. Project Title     : ${_var_title}
3. GitHub Repository : ${_var_github}
4. Author            : ${_var_author}
5. Maintainer        : ${_var_maintainer}
6. Copyright         : ${_var_copyright}
EOF

echo
read -p "Do you want to proceed? (y/N) " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    >&2 echo "Exiting..."
fi
echo

_files() {
    find . -type d \( -path ./dist -o -path ./dist-newstyle -o -path ./.direnv -o -path ./.git \) -prune -o -type f -a -not -name "run-template.sh" -print
}

_update() {
    _file="${1}"
    _fpath_def="$(echo "${_def_name}" | sed "s/\-/_/g")"
    _fpath_new="$(echo "${_var_name}" | sed "s/\-/_/g")"
    sed -i "s|${_def_github}|${_var_github}|g" "${_file}"
    sed -i "s|${_def_copyright}|${_var_copyright}|g" "${_file}"
    sed -i "s|${_def_author}|${_var_author}|g" "${_file}"
    sed -i "s|${_def_maintainer}|${_var_maintainer}|g" "${_file}"
    sed -i "s|${_def_name}|${_var_name}|g" "${_file}"
    sed -i "s|${_def_title}|${_var_title}|g" "${_file}"
    sed -i "s|${_fpath_def}|${_fpath_new}|g" "${_file}"
}

_files | sort | while read -r _path; do
    echo "Processing ${_path}"
    _update "${_path}"
done
