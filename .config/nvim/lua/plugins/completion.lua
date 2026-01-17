return {
  { -- optional blink completion source for require statements and module annotations
    'saghen/blink.cmp',
    dependencies = {
      'Kaiser-Yang/blink-cmp-avante',
      'rafamadriz/friendly-snippets'
    },
    version = '1.*',
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      -- All presets have the following mappings:
      -- C-space: Open menu or open docs if already open
      -- C-n/C-p or Up/Down: Select next/previous item
      -- C-e: Hide menu
      -- C-k: Toggle signature help (if signature.enabled = true)
      --
      -- See :h blink-cmp-config-keymap for defining your own keymap
      keymap = { preset = 'default' },

      appearance = {
        nerd_font_variant = 'mono'
      },
      -- (Default) Only show the documentation popup when manually triggered
      completion = { documentation = { auto_show = false } },

      sources = {
        default = { 'avante', 'lsp', 'path', 'snippets', 'buffer' },
        providers = {
          avante = {
            module = 'blink-cmp-avante',
            name = 'Avante',
            opts = {
            }
          }
        },
      },
      signature = { enabled = true },
      fuzzy = { implementation = 'prefer_rust_with_warning' }
    },
    opts_extend = { 'sources.default' }
  }

}
