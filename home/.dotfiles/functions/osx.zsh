#!/usr/bin/env bash

# Returns the current screenresolution
function screenresolution() {
  system_profiler SPDisplaysDataType | grep Resolution | cut -d ':' -f 2 | xargs
}
