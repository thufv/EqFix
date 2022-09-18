using System;
using System.Drawing;

namespace EqFix.Lib.Logging
{
    /// <summary>
    /// A simple logger with color support. Messages are printed to stdout.
    /// This class is implemented as a thread-safe singleton.
    /// Call property <code>Instance</code> to obtain the instance.
    /// </summary>
    public sealed class Logger
    {
        /// <summary>
        /// The internal instance, shall be singleton.
        /// </summary>
        private static Logger instance = null;
        
        /// <summary>
        /// The lock.
        /// </summary>
        private static readonly object padlock = new object();

        /// <summary>
        /// Disable construtor.
        /// </summary>
        private Logger() {}

        /// <summary>
        /// The instance, read only for callers.
        /// </summary>
        /// <value>The logger instance.</value>
        public static Logger Instance
        {
            get
            {
                lock (padlock)
                {
                    if (instance == null) {
                        instance = new Logger();
                    }
                    return instance;
                }
            }
        }

        /// <summary>
        /// Log levels. The priority is from the highest to the lowest.
        /// </summary>
        public enum LogLevel { ERROR, WARNING, FAILURE, SUCCESS, INFO, DEBUG, FINE }

        /// <summary>
        /// Get string representation for each log level.
        /// </summary>
        /// <returns>The string representation.</returns>
        private string LevelString(LogLevel level)
        {
            switch (level) {
                case LogLevel.ERROR: return "Error";
                case LogLevel.WARNING: return "Warning";
                case LogLevel.FAILURE: return "Failure";
                case LogLevel.SUCCESS: return "Success";
                case LogLevel.INFO: return "Info";
                case LogLevel.DEBUG: return "Debug";
                case LogLevel.FINE: return "Fine";
            }
            return "";
        }

        /// <summary>
        /// Get color for each log level.
        /// </summary>
        /// <returns>The color.</returns>
        private Color LevelColor(LogLevel level)
        {
            switch (level) {
                case LogLevel.ERROR: return Color.Red;
                case LogLevel.WARNING: return Color.Yellow;
                case LogLevel.FAILURE: return Color.DarkRed;
                case LogLevel.SUCCESS: return Color.DarkGreen;
                case LogLevel.INFO: return Color.White;
                case LogLevel.DEBUG: return Color.LightBlue;
                case LogLevel.FINE: return Color.Navy;
            }
            return Color.White;
        }
        
        /// <summary>
        /// Set the lowest log level that shall be displayed.
        /// </summary>
        /// <value>The lowest log level. Default<code>INFO</code>.</value>
        public LogLevel DisplayLevel { get; set; } = LogLevel.INFO;

        /// <summary>
        /// Whether to enable colorful messages.
        /// </summary>
        /// <value>Enable/disable. Default disable.</value>
        public bool ShowColor { get; set; }

        /// <summary>
        /// Display message.
        /// Format: <code>[level] msg</code>.
        /// </summary>
        /// <param name="Level">The log level.</param>
        /// <param name="msg">The log message.</param>
        public void Log(LogLevel level, string msg)
        {
            // check priority
            if (level > DisplayLevel) return;

            // display message
            var fullMessage = "[" + LevelString(level) + "] " + msg;
            if (ShowColor) {
                Colorful.Console.WriteLine(fullMessage, LevelColor(level));
            } else {
                Console.WriteLine(fullMessage);
            }
        }

        /// <summary>
        /// Display message with variable arguments supported.
        /// Equivalent to <code>Log(level, String.Format(format, args))</code>.
        /// </summary>
        /// <param name="level"></param>
        /// <param name="format"></param>
        /// <param name="args"></param>
        public void Log(LogLevel level, string format, params object[] args)
        {
            // check priority
            if (level > DisplayLevel) return;

            // format string
            var msg = String.Format(format, args);

            // display message
            var fullMessage = "[" + LevelString(level) + "] " + msg;
            if (ShowColor) {
                Colorful.Console.WriteLine(fullMessage, LevelColor(level));
            } else {
                Console.WriteLine(fullMessage);
            }
        }

        /// <summary>
        /// Display <code>ERROR</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.ERROR, msg)</code>.
        /// </summary>
        public void Error(string msg)
        {
            Log(LogLevel.ERROR, msg);
        }

        /// <summary>
        /// Display <code>ERROR</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.ERROR, format, args)</code>.
        /// </summary>
        public void Error(string format, params object[] args)
        {
            Log(LogLevel.ERROR, format, args);
        }

        /// <summary>
        /// Display <code>WARNING</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.WARNING, msg)</code>.
        /// </summary>
        public void Warning(string msg)
        {
            Log(LogLevel.WARNING, msg);
        }

        /// <summary>
        /// Display <code>WARNING</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.WARNING, format, args)</code>.
        /// </summary>
        public void Warning(string format, params object[] args)
        {
            Log(LogLevel.WARNING, format, args);
        }

        /// <summary>
        /// Display <code>FAILURE</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.FAILURE, msg)</code>.
        /// </summary>
        public void Failure(string msg)
        {
            Log(LogLevel.FAILURE, msg);
        }

        /// <summary>
        /// Display <code>FAILURE</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.FAILURE, format, args)</code>.
        /// </summary>
        public void Failure(string format, params object[] args)
        {
            Log(LogLevel.FAILURE, format, args);
        }

        /// <summary>
        /// Display <code>SUCCESS</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.SUCCESS, msg)</code>.
        /// </summary>
        public void Success(string msg)
        {
            Log(LogLevel.SUCCESS, msg);
        }

        /// <summary>
        /// Display <code>SUCCESS</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.SUCCESS, format, args)</code>.
        /// </summary>
        public void Success(string format, params object[] args)
        {
            Log(LogLevel.SUCCESS, format, args);
        }

        /// <summary>
        /// Display <code>INFO</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.INFO, msg)</code>.
        /// </summary>
        public void Info(string msg)
        {
            Log(LogLevel.INFO, msg);
        }

        /// <summary>
        /// Display <code>INFO</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.INFO, format, args)</code>.
        /// </summary>
        public void Info(string format, params object[] args)
        {
            Log(LogLevel.INFO, format, args);
        }

        /// <summary>
        /// Display <code>DEBUG</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.DEBUG, msg)</code>.
        /// </summary>
        public void Debug(string msg)
        {
            Log(LogLevel.DEBUG, msg);
        }

        /// <summary>
        /// Display <code>DEBUG</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.DEBUG, format, args)</code>.
        /// </summary>
        public void Debug(string format, params object[] args)
        {
            Log(LogLevel.DEBUG, format, args);
        }

        /// <summary>
        /// Display <code>FINE</code> messages. 
        /// A Simpler call of <code>Log(LogLevel.FINE, msg)</code>.
        /// </summary>
        public void Fine(string msg)
        {
            Log(LogLevel.FINE, msg);
        }

        /// <summary>
        /// Display <code>FINE</code> messages, with variable arguments supported.
        /// A Simpler call of <code>Log(LogLevel.FINE, format, args)</code>.
        /// </summary>
        public void Fine(string format, params object[] args)
        {
            Log(LogLevel.FINE, format, args);
        }
    }
}