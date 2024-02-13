local M = {}

local function gitentry(e)
  if e == ".gitignore" then
    return false
  end
  return string.sub(e, 1, string.len("./.git")) == "./.git"
end

local function cmd_T(opts)
  local dir = "."
  if opts.nargs == 1 then
    dir = opts.args
  end
  vim.cmd("!$TERMINAL " .. dir .. " &")
end

local function cmd_T_cmp()
  local dirs = {}
  require("plenary.scandir").scan_dir(".", {
    only_dirs = true,
    hidden = true,
    respect_gitignore = true,
    search_pattern = function(e)
      return not gitentry(e)
    end,
    on_insert = function(dir)
      table.insert(dirs, string.sub(dir, 3, #dir))
    end,
  })
  return dirs
end

local function cmd_W(opts)
  vim.cmd("!$BROWSER " .. opts.args .. " &")
end

local function cmd_W_cmp()
  local files = {}
  require("plenary.scandir").scan_dir(".", {
    add_dirs = false,
    hidden = true,
    respect_gitignore = true,
    search_pattern = function(e)
      return not gitentry(e)
    end,
    on_insert = function(file)
      table.insert(files, string.sub(file, 3, #file))
    end,
  })
  return files
end

local function user_commands()
  vim.api.nvim_create_user_command("T", cmd_T, {
    complete = cmd_T_cmp,
    nargs = "?",
  })

  vim.api.nvim_create_user_command("W", cmd_W, {
    complete = cmd_W_cmp,
    nargs = "+",
  })
end

function M.setup()
  user_commands()
end

M.setup()

return M
