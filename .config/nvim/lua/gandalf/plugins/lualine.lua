local M = { "nvim-lualine/lualine.nvim" }

M.cond = true

M.lazy = false

local function theme()
  local auto = require("lualine.themes.auto")
  if vim.g.colors_name == "default" then
    for _, group in ipairs({ "normal", "insert", "replace", "visual", "command" }) do
      auto[group].b.bg = "NvimDarkGrey3"
      auto[group].c.bg = "NvimDarkGrey2"
      auto[group].c.fg = auto.normal.b.fg
    end
  end
  return auto
end

function M.config()
  require("lualine").setup({
    options = {
      globalstatus = true,
      theme = theme(),
      component_separators = {
        left = "",
        right = "",
      },
      section_separators = {
        left = "",
        right = "",
      },
    },
    sections = {
      lualine_a = {
        "mode",
      },
      lualine_b = {},
      lualine_c = {
        {
          "filename",
          file_status = true,
          path = 1,
        },
        {
          "diagnostics",
          update_in_insert = true,
          symbols = {
            error = "E",
            warn = "W",
            info = "I",
            hint = "H",
          },
        },
      },
      lualine_x = {
        {
          "branch",
          icon = "Git:",
        },
        "diff",
      },
      lualine_y = {
        "progress",
      },
      lualine_z = {},
    },
    extensions = {
      "nvim-dap-ui",
    },
  })
end

return M
