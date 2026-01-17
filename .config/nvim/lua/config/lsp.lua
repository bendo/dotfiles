local keymap = vim.keymap

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    keymap.set('n', 'gD', vim.lsp.buf.declaration, { desc = 'Go to declaration' })
    keymap.set('n', 'gd', vim.lsp.buf.definition, { desc = 'Go to definition'})
    keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    keymap.set('n', 'gi', vim.lsp.buf.implementation, { desc = 'Go to implementation'})
    keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    keymap.set('n', '<space>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
    keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, { desc = 'Format file'})
    keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
  end
})
