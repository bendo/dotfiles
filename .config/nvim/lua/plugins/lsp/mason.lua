return {
  'williamboman/mason-lspconfig.nvim',
  opts = {
    ensure_installed = {
      'lua_ls',
      'ts_ls',
      'html',
      'cssls',
    },
  },
  dependencies = {
    {
      'williamboman/mason.nvim',
      config = true,
    },
    {
      'j-hui/fidget.nvim',
      tag = 'legacy'
    },
    'neovim/nvim-lspconfig'
  }
}
