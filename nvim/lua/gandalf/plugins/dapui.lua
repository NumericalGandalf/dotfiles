local M = { "rcarriga/nvim-dap-ui" }

M.cond = false

function M.config()
  local dapui = require("dapui")
  local widgets = require("dap.ui.widgets")

  dapui.setup({
    controls = {
      element = "repl",
      enabled = false,
      icons = {
        disconnect = "",
        pause = "",
        play = "",
        run_last = "",
        step_back = "",
        step_into = "",
        step_out = "",
        step_over = "",
        terminate = "",
      },
    },
    element_mappings = {},
    expand_lines = true,
    floating = {
      border = "single",
      mappings = {
        close = {
          "q",
          "<Esc>",
        },
      },
    },
    force_buffers = true,
    icons = {
      collapsed = "",
      current_frame = "",
      expanded = "",
    },
    layouts = {
      {
        elements = {
          {
            id = "breakpoints",
            size = 0.2,
          },
          {
            id = "stacks",
            size = 0.2,
          },
          {
            id = "watches",
            size = 0.3,
          },
          {
            id = "scopes",
            size = 0.3,
          },
        },
        position = "left",
        size = 35,
      },
      {
        elements = {
          {
            id = "repl",
            size = 1,
          },
        },
        position = "bottom",
        size = 13,
      },
    },
    mappings = {
      edit = "e",
      expand = { "<CR>", "<2-LeftMouse>" },
      open = "o",
      remove = "d",
      repl = "r",
      toggle = "t",
    },
    render = {
      indent = 1,
      max_value_lines = 100,
    },
  })

  vim.keymap.set({ "n", "v" }, "<Leader>dh", function()
    dapui.float_element()
  end)
  vim.keymap.set({ "n", "v" }, "<Leader>dv", function()
    dapui.eval()
  end)
  vim.keymap.set("n", "<Leader>df", function()
    widgets.centered_float(widgets.frames)
  end)
  vim.keymap.set("n", "<Leader>ds", function()
    widgets.centered_float(widgets.scopes)
  end)
end

return M
