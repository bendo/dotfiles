return {
  'nvim-neorg/neorg',
  lazy = false,
  version = '*',
  dependencies = {
    'nvim-treesitter/nvim-treesitter',
    'nvim-neorg/tree-sitter-norg',
    'nvim-neorg/tree-sitter-norg-meta',
  },
  config = function()
    local rocks_root = vim.fn.stdpath('data') .. '/lazy-rocks'
    local norg_parser = rocks_root .. '/tree-sitter-norg/lib/lua/5.1/parser/norg.so'
    local norg_meta_parser = rocks_root .. '/tree-sitter-norg-meta/lib/lua/5.1/parser/norg_meta.so'
    local parser_rocks = {
      rocks_root .. '/tree-sitter-norg/lib/lua/5.1/?.so',
      rocks_root .. '/tree-sitter-norg-meta/lib/lua/5.1/?.so',
    }

    package.cpath = table.concat(parser_rocks, ';') .. ';' .. package.cpath
    vim.treesitter.language.add('norg', { path = norg_parser })
    vim.treesitter.language.add('norg_meta', { path = norg_meta_parser })

    require('neorg').setup({
      load = {
        ['core.defaults'] = {},
        ['core.concealer'] = {},
        ['core.dirman'] = {},
      },
    })
  end,
}
