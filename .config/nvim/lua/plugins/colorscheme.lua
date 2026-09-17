return {
'navarasu/onedark.nvim',
    priority = 1000,
    config = function()
      vim.cmd.colorscheme 'onedark'
      vim.api.nvim_set_hl(0, '@markup.strong.markdown_inline', {
        fg = '#D19A66',
        bold = true,
      })
      vim.api.nvim_set_hl(0, '@markup.italic.markdown_inline', {
        fg = '#98C379',
        italic = true,
      })

      local markdown_heading_colors = {
        '#E06C75',
        '#D19A66',
        '#61AFEF',
        '#56B6C2',
        '#C678DD',
        '#98C379',
      }
      for level, color in ipairs(markdown_heading_colors) do
        vim.api.nvim_set_hl(0, '@markup.heading.' .. level .. '.markdown', {
          fg = color,
          bold = true,
        })
      end
    end
}
