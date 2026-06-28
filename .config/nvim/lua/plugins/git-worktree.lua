return {
  'ThePrimeagen/git-worktree.nvim',
  dependencies = {
    'nvim-telescope/telescope.nvim'
  },
  keys = {
    { '<leader>ww', desc = 'List Worktrees' },
    { '<leader>wc', desc = 'Create Git Worktrees' },
  },
  config = function ()
    require('git-worktree').setup()
    vim.keymap.set('n', '<leader>ww', function()
      require('telescope').load_extension('git_worktree')
      require('telescope').extensions.git_worktree.git_worktrees()
    end, { desc = 'List Worktrees'})
    vim.keymap.set('n', '<leader>wc', function()
      require('telescope').load_extension('git_worktree')
      require('telescope').extensions.git_worktree.create_git_worktree()
    end, { desc = 'Create Git Worktrees'})
  end
}
