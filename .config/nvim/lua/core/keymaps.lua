-- KEYMAPS

local keymap = vim.keymap

keymap.set('n', '<leader>oe', vim.cmd.Ex, { desc = '[O]pen [E]ditor' })

-- move up or down selected text
keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
keymap.set('v', 'K', ":m '<-2<CR>gv=gv")

-- keep cursor at place when joining lines
keymap.set('n', 'J', 'mzJ`z')

-- center searched text
keymap.set('n', 'n', 'nzzzv')
keymap.set('n', 'N', 'Nzzzv')

-- paste yanked text, then select and replace
vim.keymap.set('x', '<leader>p', "\"_dP", { desc = '[P]aste yanked text' })

-- quick search fix
keymap.set('n', '<C-k>', '<cmd>cnext<CR>zz')
keymap.set('n', '<C-j>', '<cmd>cprev<CR>zz')
keymap.set('n', '<leader>k', '<cmd>lnext<CR>zz')
keymap.set('n', '<leader>j', '<cmd>lprev<CR>zz')

-- make file executable
keymap.set('n', '<leader>x', '<cmd>!chmod +x %<CR>', { silent = true })

-- functional keys
keymap.set('n', '<F3>', ':set hlsearch! hlsearch?<CR>', { noremap = true, silent = true})
keymap.set('n', '<F2>', ':let &cc = &cc == "" ? "90" : ""<CR>', { noremap = true, silent = true})

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

keymap.set('n', '<leader>hf', '<cmd>%!stylish-haskell<CR>', {desc = 'Format haskell'})

keymap.set('n', '<leader>ee', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
keymap.set('n', '<leader>eq', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })
-- go to next message
keymap.set('n', ']d', function ()
  vim.diagnostic.jump({count=1, float=true})
end, { desc = 'Go to next diagnostic message' })
-- go to previous message
keymap.set('n', '[d', function ()
  vim.diagnostic.jump({count=-1, float=true})
end, { desc = 'Go to previous diagnostic message' })
