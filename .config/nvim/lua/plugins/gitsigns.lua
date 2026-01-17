return {
  {
    'lewis6991/gitsigns.nvim',
    lazy = false,
    config = function ()
      local gs = require('gitsigns')

      gs.setup {
        preview_config = {
          border = 'rounded',
        },
        on_attach = function ()
          vim.keymap.set('n', '<leader>gb', function() gs.blame_line({ full = true }) end, { desc = 'Blame line (full)' })
          vim.keymap.set('n', '<leader>gd', function() gs.diffthis('main') end, { desc = 'Diff this file against main'})
          vim.keymap.set('n', '<leader>gh', gs.preview_hunk, { desc = 'Preview hunk' })
          vim.keymap.set('n', '<leader>gt', gs.toggle_current_line_blame, { desc = 'Git toggle blame' });
          -- vim.keymap.set('n', '<leader>td', gs.preview_hunk_inline, { desc = 'Toggle deleted' });
          -- vim.keymap.set('n', '<leader>hS', gs.stage_buffer, { desc = 'Hunk stage buffer' })
          -- vim.keymap.set('n', '<leader>ha', gs.stage_hunk, { desc = 'Hunk add' })
          -- vim.keymap.set('n', '<leader>hR', gs.reset_buffer, { desc = 'Hunk reset buffer' })
        end
      }
    end
  }
}
