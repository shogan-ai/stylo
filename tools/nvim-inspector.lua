local M = {}

local stylo_path = ""

local out_buf_name = "stylo tokens dump"

local function get_scratch_buffer()
    local bufnr = vim.fn.bufnr(out_buf_name)
    if bufnr ~= -1 and vim.api.nvim_buf_is_valid(bufnr) then
        return bufnr
    end

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, out_buf_name)
    vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
    vim.api.nvim_buf_set_option(buf, "bufhidden", "hide")
    vim.api.nvim_buf_set_option(buf, "swapfile", false)
    vim.api.nvim_buf_set_option(buf, "modifiable", false)
    vim.api.nvim_buf_set_option(buf, "filetype", "text")
    return buf
end

local function open_buffer_in_window(bufnr)
    -- if a window already displays the buffer, focus it
    for _, winnr in ipairs(vim.api.nvim_list_wins()) do
        if vim.api.nvim_win_get_buf(winnr) == bufnr then
            vim.api.nvim_set_current_win(winnr)
            return
        end
    end

    -- otherwise create a new window
    vim.cmd("botright new")
    vim.api.nvim_win_set_buf(0, bufnr)
end

local function invoke_stylo(opts)
    local err_lvl = vim.log.levels.ERROR

    local stylo_exists = vim.uv.fs_stat(stylo_path)
    if not stylo_exists then
        vim.notify("stylo's binary not found", err_lvl)
        return
    end

    local is_visual

    -- Detect mode & build range/position argument
    local mode = vim.fn.mode()
    local is_visual = mode == 'v' or mode == 'V' or mode == '\22'
    local range_args

    if is_visual then
        -- Visual mode: use '< and '> marks
        -- vim.fn.col() and line() use 0 to denote invalid args and start the
        -- actual range at 1.
        local s_line = vim.fn.line("'<")
        local s_col  = vim.fn.col("'<")
        local e_line = vim.fn.line("'>")
        local e_col  = vim.fn.col("'>")
        if s_line == 0 or s_col == 0 or e_line == 0 or e_col == 0 then
            vim.notify("failed to retrieve visual range", err_lvl)
            return
        end
        range_args = {
            "--start",
            string.format("%d:%d", s_line - 1, s_col - 1),
            "--stop",
            string.format("%d:%d", e_line - 1, e_col - 1)
        }
    else
        -- Normal/Insert mode: use current cursor
        -- nvim_win_get_cursor returns {line (1-based), col (0-based)}
        local cursor = vim.api.nvim_win_get_cursor(0)
        local line = cursor[1] - 1
        local col  = cursor[2]
        range_args = {
            "--start",
            string.format("%d:%d", line, col)
        }
    end

    -- Buffer content as a string
    local bufnr = vim.api.nvim_get_current_buf()
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local buffer_content = table.concat(lines, "\n")

        -- ran into https://github.com/hrsh7th/nvim-cmp/issues/1017
        local unpack = table.unpack or unpack

    -- Synchronous call.
    local cmd = { stylo_path, unpack(range_args) }
    local job = vim.system(cmd, { stdin = buffer_content }):wait()

    if job.code ~= 0 then
        vim.notify(
            string.format("stylo exited with code %d\nstderr: %s", job.code, job.stderr),
            err_lvl
        )
        return
    end

    local output_lines
    if job.stdout == "" then
        output_lines = { "" }
    else
        output_lines = vim.split(job.stdout, "\n", { plain = true })
    end

    local out_bufnr = get_scratch_buffer()
    vim.api.nvim_buf_set_option(out_bufnr, 'modifiable', true)
    vim.api.nvim_buf_set_lines (out_bufnr, 0, -1, false, output_lines)
    vim.api.nvim_buf_set_option(out_bufnr, 'modifiable', false)
    vim.api.nvim_buf_set_option(out_bufnr, 'readonly', true)

    open_buffer_in_window(out_bufnr)
end

function M.setup(stylo_repo)
    stylo_path = vim.fs.normalize(stylo_repo .. "/_build/default/bin/dump_tokens.exe")

    vim.api.nvim_create_user_command("StyloInspectTokens", invoke_stylo, {
        desc = "Show tokens of selected node / node under the cursor",
        nargs = 0,
        range = "%"
    })
end

return M

