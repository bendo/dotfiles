local keymap = vim.keymap

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', { clear = true }),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local function map(mode, lhs, rhs, desc)
      keymap.set(mode, lhs, rhs, {
        buffer = ev.buf,
        desc = desc,
      })
    end

    map('n', 'gD', vim.lsp.buf.declaration, 'Go to declaration')
    map('n', 'gd', vim.lsp.buf.definition, 'Go to definition')
    map('n', 'K', vim.lsp.buf.hover, 'Hover documentation')
    map('n', 'gi', vim.lsp.buf.implementation, 'Go to implementation')
    map('n', '<space>wa', vim.lsp.buf.add_workspace_folder, 'Add workspace folder')
    map('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, 'Remove workspace folder')
    map('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, 'List workspace folders')
    map('n', '<space>D', vim.lsp.buf.type_definition, 'Go to type definition')
    map('n', '<space>rn', vim.lsp.buf.rename, 'Rename symbol')
    map('n', 'gr', vim.lsp.buf.references, 'Find references')
    map('n', '<space>f', function()
      vim.lsp.buf.format({ async = true })
    end, 'Format file')
    map({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, 'Code action')
  end
})
