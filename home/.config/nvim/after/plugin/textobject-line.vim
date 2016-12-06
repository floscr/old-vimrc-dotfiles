" Surround does not work well will lowercase il or al
" ysbal -> Add brackets around line
" ysbiL -> Add brackets in line
call textobj#user#plugin('line', {
\      '-': {
\        'select-a': 'aL', 'select-a-function': 'textobj#line#select_a',
\        'select-i': 'iL', 'select-i-function': 'textobj#line#select_i',
\      },
\    })
