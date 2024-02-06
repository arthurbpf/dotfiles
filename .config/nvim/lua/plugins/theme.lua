return {
    { "catppuccin/nvim", name = "catppuccin", priority = 1000,

    config = function()
    vim.cmd.colorscheme 'catppuccin-mocha'
  end,
  },
    {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = '|',
        section_separators = '',
      },
    },
  }
}
