sed -e 's/[^-0-9]/ /g' -e 's/  */ /g' <input.txt | tr ' ' '\n'
