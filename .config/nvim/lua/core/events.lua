local autocmd = vim.api.nvim_create_autocmd

local augroup = vim.api.nvim_create_augroup

local yank_group = augroup('YankHighlight', { clear = true })

autocmd('TextYankPost', {
  group = yank_group,
  pattern = '*',
  callback = function()
    vim.highlight.on_yank()
  end
})

-- .ino files are treated as c++ files
autocmd({'BufRead', 'BufNewFile'}, {
  pattern = '*.ino',
  callback = function()
    vim.bo.filetype = 'cpp'
  end
})
