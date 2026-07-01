return {
  "obsidian-nvim/obsidian.nvim",
  version = "*", -- use latest release, remove to use latest commit
  lazy = false,

  init = function ()
    vim.o.conceallevel = 1
  end,

  ---@module 'obsidian'
  ---@type obsidian.config
  keys = {
    { "<leader>ot", "<cmd>Obsidian today<CR>", desc = "Obsidian today" },
    { "<leader>oy", "<cmd>Obsidian today -1<CR>", desc = "Obsidian yesterday" },

    { "<leader>on", "<cmd>Obsidian new<CR>", desc = "Obsidian new note" },
    { "<leader>oq", "<cmd>Obsidian quick_switch<CR>", desc = "Obsidian quick switch" },
    { "<leader>os", "<cmd>Obsidian search<CR>", desc = "Obsidian search" },

    { "<leader>of", "<cmd>Obsidian follow_link<CR>", desc = "Obsidian follow link" },
    { "<leader>ob", "<cmd>Obsidian backlinks<CR>", desc = "Obsidian backlinks" },
    { "<leader>ol", "<cmd>Obsidian links<CR>", desc = "Obsidian links" },

    { "<leader>or", "<cmd>Obsidian rename<CR>", desc = "Obsidian rename" },
    { "<leader>oo", "<cmd>Obsidian open<CR>", desc = "Open in Obsidian app" },
    { "<leader>og", "<cmd>Obsidian tags<CR>", desc = "Obsidian tags" },
    { "<leader>om", "<cmd>Obsidian template<CR>", desc = "Obsidian template" },
  },

  opts = function()
    return {
      legacy_commands = false, -- this will be removed in 4.0.0
      ui = {
        enable = false,
      },
      workspaces = {
        {
          name = "notes",
          path = "~/notes",
        },
        {
          name = "work",
          path = "~/code/obsidian-vault/work",
        },
      },
      note_id_func = require("obsidian.builtin").title_id,
      templates = {
        folder = "templates",
      },
    }
  end,
}
