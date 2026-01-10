#!/usr/bin/env bash

set -Eeuo pipefail

###############
## VARIABLES ##
###############

_clean=false
_cabal="$(command -v cabal)"

#####################
## TERMINAL COLORS ##
#####################

if [[ -t 1 ]] && command -v tput >/dev/null 2>&1 && [[ "$(tput colors 2>/dev/null || echo 0)" -ge 8 ]]; then
  BOLD="$(tput bold)"
  RESET="$(tput sgr0)"
  RED="$(tput setaf 1)"
  GREEN="$(tput setaf 2)"
  BLUE="$(tput setaf 4)"
else
  BOLD=""
  RESET=""
  RED=""
  GREEN=""
  BLUE=""
fi

#######################
## UTILITY FUNCTIONS ##
#######################

_usage() {
  echo "Usage: ${0} [OPTIONS]"
  echo ""
  echo "  Runs all checks and tests for the project."
  echo ""
  echo "Options:"
  echo "  -c, --clean       Clean the project before running tests."
  echo "  -h, --help        Show this help message and exit."
}

_suc() {
  printf "%s%s✅ %s%s\n" "${BOLD}" "${GREEN}" "${1}" "${RESET}"
}

_err() {
  printf "%s%s❌ %s%s\n" "${BOLD}" "${RED}" "${1}" "${RESET}"
}

_get_now_ms() {
  date +%s%3N
}

_diff_ms_to_s() {
  local _ms="${1}"
  printf '%d.%03d' "$((_ms / 1000))" "$((_ms % 1000))"
}

_run_check() {
  local _title="${1}"
  shift

  printf "%s%s🔵 Running %s%s " "${BOLD}" "${BLUE}" "${_title}" "${RESET}"

  local _ms_since
  local _ms_until
  local _ms_total
  local _captured

  _ms_since="$(_get_now_ms)"

  if _captured="$(chronic -- "${@}" 2>&1)"; then
    _ms_until="$(_get_now_ms)"
    _ms_total=$((_ms_until - _ms_since))
    _suc "$(_diff_ms_to_s "${_ms_total}")s"
  else
    _ms_until="$(_get_now_ms)"
    _ms_total=$((_ms_until - _ms_since))
    _err "$(_diff_ms_to_s "${_ms_total}")s"
    >&2 printf "%s\n" "${_captured}"
    exit 1
  fi
}

####################################
## COMMAND-LINE ARGUMENTS PARSING ##
####################################

# cabal external command support
if [[ -n "${CABAL:-}" ]]; then
  _cabal="${CABAL}"
  shift
fi

while [[ $# -gt 0 ]]; do
  case "${1}" in
  --)
    shift
    break
    ;;
  -c | --clean)
    _clean=true
    shift
    ;;
  -h | --help)
    _usage
    exit 0
    ;;
  -*)
    _usage
    echo ""
    >&2 _err "Invalid option: ${1}"
    exit 1
    ;;
  *)
    break
    ;;
  esac
done

###############
## PROCEDURE ##
###############

_script_start_ms="$(_get_now_ms)"

${_clean} && _run_check "clean" "${_cabal}" clean && _run_check "v1-clean" "${_cabal}" v1-clean

_run_check "hpack (v$(hpack --numeric-version))" \
  hpack

_run_check "nixfmt (v$(nixfmt --numeric-version))" \
  find . -type f -iname "*.nix" -exec nixfmt --check {} +

_run_check "statix (v$(statix --version | cut -f2 -d" "))" \
  statix check

_run_check "shfmt (v$(shfmt --version))" \
  find . -type f -iname "*.sh" -exec shfmt --diff {} +

_run_check "shellcheck (v$(shellcheck --version | grep "^version" | head -n1 | cut -f2 -d" "))" \
  find . -type f -iname "*.sh" -exec shellcheck {} +

_run_check "prettier (v$(prettier --version))" \
  prettier --check .

_run_check "taplo lint (v$(taplo --version | cut -f2 -d" "))" \
  taplo lint

_run_check "taplo format (v$(taplo --version | cut -f2 -d" "))" \
  taplo format --check

_run_check "fourmolu (v$(fourmolu --version | head -n1 | cut -f2 -d" "))" \
  fourmolu --quiet --mode check app/ src/ test/

_run_check "hlint (v$(hlint --numeric-version))" \
  hlint app/ src/ test/

_run_check "cabal build (v$("${_cabal}" --numeric-version))" \
  "${_cabal}" build -O0

_run_check "cabal run (v$("${_cabal}" --numeric-version))" \
  "${_cabal}" run "$(yq ".executables | keys | .[0]" package.yaml)" -O0 -- --version

_run_check "cabal test (v$("${_cabal}" --numeric-version))" \
  "${_cabal}" v1-test --ghc-options="-O0"

_run_check "weeder (v$(weeder --version | head -n1 | cut -f3 -d" "))" \
  weeder

_run_check "stan ($(stan --version | head -n1 | cut -f2 -d" "))" \
  stan --hiedir ./dist-newstyle

_run_check "cabal haddock (v$("${_cabal}" --numeric-version))" \
  "${_cabal}" haddock -O0 \
  --haddock-quickjump \
  --haddock-hyperlink-source \
  --haddock-html-location="https://hackage.haskell.org/package/\$pkg-\$version/docs"

_suc "All checks passed in $(_diff_ms_to_s $(($(_get_now_ms) - _script_start_ms)))s."
