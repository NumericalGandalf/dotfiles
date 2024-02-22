local M = { "nvim-lualine/lualine.nvim" }

M.dependencies = { "nvim-tree/nvim-web-devicons" }

M.cond = true

M.lazy = false

local function position()
  local cur = vim.fn.line(".")
  local total = vim.fn.line("$")
  local pog = "Top"
  if cur == total then
    pog = "Bot"
  elseif cur ~= 1 then
    pog = string.format("%2d%%%%", math.floor(cur / total * 100))
  end
  return cur .. ":" .. vim.fn.virtcol(".") .. " (" .. pog .. ")"
end

function M.config()
  require("lualine").setup({
    options = {
      globalstatus = true,
      component_separators = {
        left = "",
        right = "│",
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
      lualine_b = {
        {
          "branch",
          icon = "",
        },
      },
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
        "diff",
      },
      lualine_x = {
        "filetype",
        "filesize",
        "encoding",
        "fileformat",
        "hostname",
      },
      lualine_y = {
        position,
      },
      lualine_z = {},
    },
    extensions = {
      "nvim-dap-ui",
    },
  })
end

return M
