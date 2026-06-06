return {
  'haskell/stylish-haskell',
  {
    'neovim/nvim-lspconfig',
    config = function()
      vim.lsp.config('hls', {
        cmd = {
          '/home/bendo/.ghcup/hls/2.13.0.0/bin/haskell-language-server-9.12.2',
          '--lsp',
        },
      })
      vim.lsp.enable('hls')
    end,
  },
}
