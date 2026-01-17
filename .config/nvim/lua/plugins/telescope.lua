return {
  'nvim-telescope/telescope.nvim', version = '*',
  dependencies = {
    'nvim-lua/plenary.nvim',
    -- optional but recommended
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  },
  config = function ()
    vim.keymap.set('n', '<leader>sf', require('telescope.builtin').find_files, { desc = 'Search files' })
    vim.keymap.set('n', '<leader>si', require('telescope.builtin').git_files, { desc = 'Search git files' })
    vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = 'Search help' })
    vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = 'Search current word' })
    vim.keymap.set('n', '<leader>sg', require('telescope.builtin').live_grep, { desc = 'Search by grep' })
    vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = 'Search diagnostics' })
    vim.keymap.set('n', '<leader>sp', function ()
      require('telescope.builtin').grep_string {
        search = vim.fn.input('Grep > ')
      }
    end, { desc = 'Search word in the project' })
    vim.keymap.set('n', '<leader>gc', require('telescope.builtin').git_commits, { desc = 'Git commits' })
    vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = 'Find recently opened files' })
    vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = 'Find existing buffers' })
    vim.keymap.set('n', '<leader>en', function ()
      require('telescope.builtin').find_files {
        cwd = vim.fn.stdpath('config')
      }
    end, { desc = 'Edit neovim config' })
    vim.keymap.set('n', '<leader>gs', require('telescope.builtin').git_status, { desc = 'Git status' })
    vim.keymap.set('n', '<leader>/', function()
      -- You can pass additional configuration to telescope to change theme, layout, etc.
      require('telescope.builtin').current_buffer_fuzzy_find(require(
        'telescope.themes').get_dropdown {
          winblend = 10,
          previewer = false,
        })
    end, { desc = '[/] Fuzzily search in current buffer' })
  end
}
