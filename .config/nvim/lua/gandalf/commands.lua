local M = {}

local function gitentry(e)
  if e == ".gitignore" then
    return false
  end
  return string.sub(e, 1, string.len("./.git")) == "./.git"
end

local function command_T(opts)
  local dir = "."
  if opts.nargs == 1 then
    dir = opts.args
  end
  vim.cmd("!$TERMINAL " .. dir .. " &")
end

local function command_T_complete()
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

local function command_W(opts)
  vim.cmd("!$BROWSER " .. opts.args .. " &")
end

local function command_W_complete()
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
  vim.api.nvim_create_user_command("T", command_T, {
    complete = command_T_complete,
    nargs = "?",
  })

  vim.api.nvim_create_user_command("W", command_W, {
    complete = command_W_complete,
    nargs = "+",
  })
end

function M.setup()
  user_commands()
end

M.setup()

return M
