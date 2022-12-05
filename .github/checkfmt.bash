#!/bin/bash

print() {
  local line=$1
  local text=$2

  echo "$file:$line: `basename $file` $message"
  echo "$text"
}

blank='^[[:space:]]*$'

sbt -no-colors clean scalafmtCheck | while IFS= read ll
do

  # Keep all lines in logs
  echo "$ll"

  # Skip lines which are not [warn]
  if ! [[ "$ll" =~ ^\[warn\] ]]; then
    continue
  fi
  # Remove this warn to ease process
  l=$(sed 's/^\[warn\] //' <<< "$ll")

  # Match start of mis-formatted file
  if [[ "$l" =~ ^scalafmt:\ $PWD/([^[:space:]]+)\ (isn\'t\ formatted\ properly\!)$ ]]
  then
    file="${BASH_REMATCH[1]}"
    message="${BASH_REMATCH[2]}"
  fi

  # Debug
  #echo "debug $linea:$lineb:$l"
  #echo "blank $blanklines"
  #echo "removed $removedlines"

  # Match start of code chunk
  if [[ "$l" =~ ^@@\ -([[:digit:]]+),[[:digit:]]+\ \+[[:digit:]]+,[[:digit:]]+\ @@$ ]]
  then
    # current line in actual file
    linea="${BASH_REMATCH[1]}"
    # line of actual file matching line of virtual formatted file
    lineb="$linea"
    # count of empty lines for resync between paragraphs
    blanklines=0
    # count of removed lines to add more removed lines after block
    removedlines=0
  fi

  # Match diff line which is addition, kept or deleted ('+', ' ' or '-')
  if [[ "$l" =~ ^([[:space:]+-])(.*)$ ]]; then
    linetype="${BASH_REMATCH[1]}"
    linecode="${BASH_REMATCH[2]}"

    # Skip if diff introducing filename
    if [[ "$l" =~ ^\+\+\+\ b/ ]] || [[ "$l" =~ ^---\ a/ ]]; then
      continue
    fi

    # Manage blank lines
    if [[ "$linetype" == - ]]; then
      [[ "$linecode" =~ $blank ]] && blanklines=$((blanklines + 1))
    else # End of removed line, flush
      # The current line will be printed in print phase
      if [[ "$linetype" == + ]] && [[ "$linecode" =~ $blank ]]; then
        blanklines=$((blanklines - 1))
      fi
      # Resync lines
      while (($blanklines > 0)); do
        print "$lineb" "-"
        blanklines=$((blanklines - 1))
        lineb=$((lineb + 1))
      done
    fi

    # Count removed lines (blank lines are managed by blanklines)
    if ! [[ "$linecode" =~ $blank ]]; then
      if [[ "$linetype" == - ]]; then
        removedlines=$((removedlines + 1))
      elif [[ "$linetype" == + ]]; then
        removedlines=$((removedlines - 1))
      fi
    fi

    # If it is a new line from new version, print it
    [[ "$linetype" == + ]] && print "$lineb" "$l"

    # If it is an kept line, check if there was no remaining deletions
    if [[ "$linetype" == " " ]]; then
      if (($removedlines > 0)); then
        print "$lineb" "-"
        removedlines=$((removedlines - 1))
      fi
    fi

    # Update line numbers
    if [[ "$linetype" == + ]]; then
      # Lines are appended in + to line above OR in modified block
      # so lineb cannot join linea this way
      if (( $lineb + 1 < $linea )); then
        lineb=$((lineb + 1))
      fi
    elif [[ "$linetype" == - ]]; then
      # Increment end of modified block
      linea=$((linea + 1))
    else # linetype is ' '
      # resync
      linea=$((linea + 1))
      lineb=$linea
    fi
  fi
done
