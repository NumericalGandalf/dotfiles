local M = { "nvim-lualine/lualine.nvim" }

M.cond = true

M.lazy = false

local function theme()
  local auto = require("lualine.themes.auto")
  if vim.g.colors_name == "dalf" then
    for _, group in ipairs({ "normal", "insert", "replace", "visual", "command" }) do
      auto[group].b.bg = "NvimDarkGrey4"
      auto[group].c.bg = "NvimDarkGrey3"
      auto[group].c.fg = auto.normal.b.fg
    end
  end
  return auto
end

local function bufnu()
  return "[" .. vim.api.nvim_get_current_buf() .. "]"
end

local function progress()
  local cur = vim.fn.line(".")
  local total = vim.fn.line("$")
  local prog = "Top"
  if cur == total then
    prog = "Bot"
  elseif cur ~= 1 then
    prog = string.format("%2d%%%%", math.floor(cur / total * 100))
  end
  return "(" .. prog .. ")"
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
        bufnu,
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
        progress,
      },
      lualine_y = {},
      lualine_z = {},
    },
    extensions = {
      "nvim-dap-ui",
    },
  })
end
M.config()
return M
