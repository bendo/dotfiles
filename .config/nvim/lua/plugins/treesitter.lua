return {
  {
    'nvim-treesitter/nvim-treesitter',
    lazy = false,
    branch = 'main',
    build = ':TSUpdate',

    config = function ()
      local ts = require('nvim-treesitter')

      ts.setup({
        install_dir = vim.fn.stdpath("data") .. "/site",
      })

      ts.install({ 'c', 'cpp', 'lua', 'python', 'rust', 'haskell', 'html', 'java', 'javascript', 'tsx', 'typescript', 'vimdoc', 'vim' })

      -- auto-start highlights & indentation
      vim.api.nvim_create_autocmd('FileType', {
        group = vim.api.nvim_create_augroup('Custom_enable_treesitter_features', {}),
        callback = function(args)
          local buf = args.buf
          local filetype = args.match

          -- checks if a parser exists for the current language
          local language = vim.treesitter.language.get_lang(filetype) or filetype
          if not vim.treesitter.language.add(language) then
            return
          end

          -- highlights
          vim.treesitter.start(buf, language)

          -- indent
          vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          -- folding
          vim.wo.foldmethod = 'expr'
          vim.wo.foldlevel = 99
          vim.wo.foldexpr   = 'v:lua.vim.treesitter.foldexpr()'
        end
      })
    end
  }
}
