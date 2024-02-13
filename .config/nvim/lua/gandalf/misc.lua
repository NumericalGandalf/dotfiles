local M = {}

local function cmd_T(opts)
  vim.cmd("!$TERMINAL " .. opts.args .. " &")
end

local function cmd_T_cmp()
  return require("plenary.scandir").scan_dir(".", {
    only_dirs = true,
    hidden = true,
    search_pattern = "/*/",
  })
end

local function user_cmds()
  vim.api.nvim_create_user_command("T", cmd_T, {
    complete = cmd_T_cmp,
    nargs = 1,
  })
end

function M.setup()
  user_cmds()
end

M.setup()

return M
