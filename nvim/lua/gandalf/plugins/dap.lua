local A = { "mfussenegger/nvim-dap" }

A.config = function()
  local dap = require("dap")

  vim.keymap.set("n", "<leader>dc", function()
    dap.continue()
  end)
  vim.keymap.set("n", "<leader>dr", function()
    dap.restart()
  end)
  vim.keymap.set("n", "<leader>de", function()
    dap.terminate()
  end)
  vim.keymap.set("n", "<leader>do", function()
    dap.step_over()
  end)
  vim.keymap.set("n", "<leader>di", function()
    dap.step_into()
  end)
  vim.keymap.set("n", "<leader>dO", function()
    dap.step_out()
  end)
  vim.keymap.set("n", "<leader>db", function()
    dap.toggle_breakpoint()
  end)
  vim.keymap.set("n", "<leader>dt", function()
    dap.repl.toggle()
  end)

  dap.adapters.python = function(cb, config)
    if config.request == "attach" then
      local port = (config.connect or config).port
      local host = (config.connect or config).host or "127.0.0.1"
      cb({
        type = "server",
        port = assert(
          port,
          "`connect.port` is required for a python `attach` configuration"
        ),
        host = host,
        options = {
          source_filetype = "python",
        },
      })
    else
      cb({
        type = "executable",
        command = "/usr/bin/python",
        args = { "-m", "debugpy.adapter" },
        options = {
          source_filetype = "python",
        },
      })
    end
  end
  dap.configurations.python = {
    {
      type = "python",
      request = "launch",
      name = "Launch Entry (main.py)",
      program = vim.fn.getcwd() .. "/src/main.py",
      pythonPath = "/usr/bin/python",
    },
  }

  if not require("gandalf.plugins.dapui").cond then
    return
  end

  local dapui = require("dapui")

  dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
  end
  dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
  end
  dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
  end
end

return A
