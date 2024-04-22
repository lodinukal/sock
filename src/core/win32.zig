const std = @import("std");

const core = @import("core.zig");

const InterfaceWindow = @import("Window.zig");

/// only safe to interact with from the calling thread
pub const Window = struct {
    h_wnd: std.os.windows.HWND,

    fn getInterface(self: *Window) *InterfaceWindow {
        return @fieldParentPtr("internal_window", self);
    }

    pub const class_name = std.unicode.utf8ToUtf16LeStringLiteral("sock_window");
    var registered_class: bool = false;
    fn registerWindowClass() void {
        const h_instance: std.os.windows.HINSTANCE = @ptrCast(getInstance());
        var wc = std.mem.zeroes(WNDCLASSW);
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = wndProc;
        wc.hInstance = h_instance;
        wc.lpszClassName = class_name;
        wc.hCursor = LoadCursorW(null, IDC_ARROW);
        wc.hbrBackground = null;
        wc.hIcon = LoadIconW(h_instance, IDI_APPLICATION);
        _ = RegisterClassW(&wc);
    }

    fn getStyles(style: InterfaceWindow.Style) std.os.windows.DWORD {
        var result: std.os.windows.DWORD = 0;
        if (style.visible) {
            result |= WS_VISIBLE;
        }
        if (style.resizable) {
            result |= WS_THICKFRAME | WS_MAXIMIZEBOX | WS_MINIMIZEBOX;
        }
        if (style.caption) {
            result |= WS_CAPTION;
        }
        if (style.system_menu) {
            result |= WS_SYSMENU;
        }
        return result;
    }

    const window_struct_prop = std.unicode.utf8ToUtf16LeStringLiteral("window");

    pub fn init(
        self: *Window,
        properties: InterfaceWindow.Properties,
        style: InterfaceWindow.Style,
    ) void {
        if (!registered_class) {
            registerWindowClass();
            registered_class = true;
        }

        const temp = core.tallocator();
        defer core.treset();

        const name = std.unicode.utf8ToUtf16LeAllocZ(temp, properties.title) catch @panic("failed to convert title to utf8");

        if (CreateWindowExW(
            0,
            class_name,
            name,
            getStyles(style),
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            @intCast(properties.width),
            @intCast(properties.height),
            null,
            null,
            @ptrCast(getInstance()),
            null,
        )) |h_wnd| {
            self.h_wnd = h_wnd;
        } else @panic("failed to create window");
        std.debug.assert(SetPropW(self.h_wnd, window_struct_prop, @ptrCast(self)));

        // if (style.visible) {
        //     _ = ShowWindow(self.h_wnd, SW_SHOWNORMAL);
        // }
    }

    pub fn deinit(self: *Window) void {
        _ = DestroyWindow(self.h_wnd);
    }

    /// returns false if the application should exit
    pub fn pumpMessages() bool {
        var msg = std.mem.zeroes(MSG);
        if (PeekMessageW(&msg, null, 0, 0, PM_REMOVE)) {
            switch (msg.message) {
                WM_QUIT => {
                    return false;
                },
                else => {
                    _ = TranslateMessage(&msg);
                    _ = DispatchMessageW(&msg);
                },
            }
        }
        return true;
    }

    fn wndProc(
        hWnd: std.os.windows.HWND,
        uMsg: std.os.windows.UINT,
        wParam: std.os.windows.WPARAM,
        lParam: std.os.windows.LPARAM,
    ) callconv(.C) std.os.windows.LRESULT {
        const window: ?*Window = @alignCast(@ptrCast(GetPropW(hWnd, window_struct_prop)));
        if (window == null) {
            return DefWindowProcW(hWnd, uMsg, wParam, lParam);
        }

        switch (uMsg) {
            WM_CLOSE => {
                window.?.getInterface().should_close = true;
                return 0;
            },
            WM_DESTROY => {
                _ = PostQuitMessage(0);
                return 0;
            },
            else => {
                return DefWindowProcW(hWnd, uMsg, wParam, lParam);
            },
        }
    }

    pub fn getNativeWindow(self: *Window) *anyopaque {
        return @ptrCast(self.h_wnd);
    }

    pub fn getDimensions(self: *Window) struct { u32, u32 } {
        var rect = std.mem.zeroes(std.os.windows.RECT);
        _ = GetClientRect(self.h_wnd, &rect);
        return .{ @intCast(rect.right), @intCast(rect.bottom) };
    }
};

pub fn getInstance() std.os.windows.HINSTANCE {
    return @ptrCast(GetModuleHandleW(null));
}

// all of the win32 bindings used
pub extern "Kernel32" fn GetModuleHandleW(
    lpModuleName: ?std.os.windows.LPCSTR,
) callconv(.C) std.os.windows.HMODULE;

extern "User32" fn DefWindowProcW(
    hWnd: std.os.windows.HWND,
    uMsg: std.os.windows.UINT,
    wParam: std.os.windows.WPARAM,
    lParam: std.os.windows.LPARAM,
) callconv(.C) std.os.windows.LRESULT;

extern "User32" fn LoadCursorW(
    hInstance: ?std.os.windows.HINSTANCE,
    lpCursorName: std.os.windows.LPCWSTR,
) callconv(.C) std.os.windows.HCURSOR;

extern "User32" fn LoadIconW(
    hInstance: std.os.windows.HINSTANCE,
    lpIconName: std.os.windows.LPCWSTR,
) callconv(.C) std.os.windows.HICON;

extern "User32" fn RegisterClassW(
    lpWndClass: *WNDCLASSW,
) callconv(.C) std.os.windows.ATOM;

extern "User32" fn CreateWindowExW(
    dwExStyle: std.os.windows.DWORD,
    lpClassName: std.os.windows.LPCWSTR,
    lpWindowName: std.os.windows.LPCWSTR,
    dwStyle: std.os.windows.DWORD,
    x: i32,
    y: i32,
    nWidth: i32,
    nHeight: i32,
    hWndParent: ?std.os.windows.HWND,
    hMenu: ?std.os.windows.HMENU,
    hInstance: std.os.windows.HINSTANCE,
    lpParam: ?std.os.windows.LPVOID,
) callconv(.C) ?std.os.windows.HWND;

extern "User32" fn PeekMessageW(
    lpMsg: *MSG,
    hWnd: ?std.os.windows.HWND,
    wMsgFilterMin: std.os.windows.UINT,
    wMsgFilterMax: std.os.windows.UINT,
    wRemoveMsg: std.os.windows.UINT,
) callconv(.C) bool; // std.os.windows.BOOL

extern "User32" fn TranslateMessage(
    lpMsg: *MSG,
) callconv(.C) bool; // std.os.windows.BOOL

extern "User32" fn DispatchMessageW(
    lpMsg: *MSG,
) callconv(.C) std.os.windows.LRESULT;

extern "User32" fn ShowWindow(
    hWnd: std.os.windows.HWND,
    nCmdShow: std.os.windows.INT,
) callconv(.C) bool; // std.os.windows.BOOL

extern "User32" fn DestroyWindow(
    hWnd: std.os.windows.HWND,
) callconv(.C) bool; // std.os.windows.BOOL

extern "User32" fn PostQuitMessage(
    nExitCode: std.os.windows.INT,
) callconv(.C) void;

extern "User32" fn GetPropW(
    hWnd: std.os.windows.HWND,
    lpString: std.os.windows.LPCWSTR,
) callconv(.C) std.os.windows.HANDLE;

extern "User32" fn SetPropW(
    hWnd: std.os.windows.HWND,
    lpString: std.os.windows.LPCWSTR,
    hData: std.os.windows.HANDLE,
) callconv(.C) bool; // std.os.windows.BOOL

extern "User32" fn GetClientRect(
    hWnd: std.os.windows.HWND,
    lpRect: *std.os.windows.RECT,
) callconv(.C) bool; // std.os.windows.BOOL

const WNDCLASSW = extern struct {
    style: std.os.windows.UINT,
    lpfnWndProc: WNDPROC,
    cbClsExtra: std.os.windows.INT,
    cbWndExtra: std.os.windows.INT,
    hInstance: std.os.windows.HINSTANCE,
    hIcon: std.os.windows.HICON,
    hCursor: std.os.windows.HCURSOR,
    hbrBackground: ?std.os.windows.HBRUSH,
    lpszMenuName: std.os.windows.LPCWSTR,
    lpszClassName: std.os.windows.LPCWSTR,
};

const WNDPROC = *const fn (
    hWnd: std.os.windows.HWND,
    uMsg: std.os.windows.UINT,
    wParam: std.os.windows.WPARAM,
    lParam: std.os.windows.LPARAM,
) callconv(.C) std.os.windows.LRESULT;

const MSG = extern struct {
    hWnd: std.os.windows.HWND,
    message: std.os.windows.UINT,
    wParam: std.os.windows.WPARAM,
    lParam: std.os.windows.LPARAM,
    time: std.os.windows.DWORD,
    pt: std.os.windows.POINT,
};

const CS_HREDRAW = 0x0002;
const CS_VREDRAW = 0x0001;

const IDC_ARROW = makeIntResourceW(32512);
const WHITE_BRUSH = makeIntResourceA(0);

const CW_USEDEFAULT = @as(i32, -2147483648);

pub const WM_NULL = @as(u32, 0);
pub const WM_CREATE = @as(u32, 1);
pub const WM_DESTROY = @as(u32, 2);
pub const WM_MOVE = @as(u32, 3);
pub const WM_SIZE = @as(u32, 5);
pub const WM_ACTIVATE = @as(u32, 6);
pub const WM_PAINT = @as(u32, 15);
pub const WM_CLOSE = @as(u32, 16);
pub const WM_QUIT = @as(u32, 18);

const SW_HIDE = 0;
const SW_SHOWNORMAL = 1;

const WS_THICKFRAME = 0x00040000; // rezieable
const WS_VISIBLE = 0x10000000; // visible
const WS_ICONIC = 0x20000000; // minimized
const WS_CAPTION = 0x00C00000; // title bar
const WS_SYSMENU = 0x00080000; // system menu
const WS_MINIMIZEBOX = 0x00020000; // minimize button
const WS_MAXIMIZEBOX = 0x00010000; // maximize button

const PM_REMOVE = 0x0001;

const IDI_APPLICATION = makeIntResourceW(32512);

pub fn makeIntResourceA(value: std.os.windows.INT) std.os.windows.LPSTR {
    return @ptrCast(@as(*std.os.windows.ULONG_PTR, @ptrFromInt(@as(std.os.windows.WORD, @intCast(value)))));
}

pub fn makeIntResourceW(value: std.os.windows.INT) std.os.windows.LPWSTR {
    return @ptrCast(@as(*std.os.windows.ULONG_PTR, @ptrFromInt(@as(std.os.windows.WORD, @intCast(value)))));
}
