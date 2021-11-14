#r "C:\Program Files\workspacer\workspacer.Shared.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"

using System;
using System.Diagnostics;
using SD = System.Drawing;
using System.Net;
using System.Text;
using System.IO;
using System.Xml;
using System.Timers;
using RE = System.Text.RegularExpressions;

using workspacer;
using workspacer.Bar;
using workspacer.Bar.Widgets;
using workspacer.ActionMenu;
using workspacer.FocusIndicator;

public record WeatherData(int Temperature);

class WeatherWidget : BarWidgetBase {
    private int _interval;
    private Timer _timer;
    private string _stationID;
    private string _stationName;
    private WeatherData _data;

    public WeatherWidget(string stationName, string stationID, int interval)
    {
        _stationName = stationName;
        _stationID = stationID;
        _interval = interval;
    }

    public override IBarWidgetPart[] GetParts() 
    {
        Color red = Color.Red;
        Color green = Color.Green;
        Color currentColor = red;

        string formatted = String.Format("{0}: {1}°C", _stationName, _data.Temperature);

        if (_data.Temperature < 24) 
        {
            currentColor = green;
        }
        return Parts(Part(formatted, currentColor, fontname: FontName));
    }

    public override void Initialize()
    {
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            _data = WeatherData(_stationID);
            Context.MarkDirty();
        };
        _timer.Enabled = true;
    }

    public WeatherData WeatherData(string stationID)
    {
        string contents = FetchText(stationID);
        return ParseWeatherTxt(contents);
    }

    private string FetchText(string stationID)
    {
        string uri = "https://tgftp.nws.noaa.gov/data/observations/metar/decoded/" + stationID + ".TXT";
        HttpWebRequest request = (HttpWebRequest) WebRequest.Create(uri);
        using (var response = (HttpWebResponse) request.GetResponse())
        {
            var encoding = Encoding.GetEncoding(response.CharacterSet);

            using (var responseStream = response.GetResponseStream())
            using (var reader = new StreamReader(responseStream, encoding))
                return reader.ReadToEnd();
        }
    }

    private WeatherData ParseWeatherTxt(string textContents) 
    {
        string pattern = @"Temperature:.*\((?<temperature>\d+) C\)";
        RE.Regex rg = new RE.Regex(pattern);
        RE.MatchCollection matches = rg.Matches(textContents);

        // TODO guard against more matches
        RE.Match match = matches[0];
        RE.GroupCollection groups = match.Groups;
        return new WeatherData(Int16.Parse(groups["temperature"].Value));
    }

}

class UVWidget : BarWidgetBase {
    private int _interval;
    private Timer _timer;
    private string _locid;
    private float _uv;

    public UVWidget(string locid, int interval)
    {
        _locid = locid;
        _interval = interval;
    } 

    public override IBarWidgetPart[] GetParts()
    {
        Color red = Color.Red;
        Color green = Color.Green;
        Color currentUVColor = red; // it's always bad ;)
        string formatted = String.Format("UV: {0}", _uv);

        if (_uv < 2.5) 
        {
            currentUVColor = green;
        }
        return Parts(Part(formatted, currentUVColor, fontname: FontName));
    }

    public override void Initialize()
    {
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            _uv = UVData(_locid);
            Context.MarkDirty();
        };
        _timer.Enabled = true;
    }

    public float UVData(string locid)
    {
        string contents = FetchUVXML();
        XmlDocument doc = GetUVXmlDocument(contents);
        string query = "/stations/location/name[contains(., '" + locid + "')]/../index";
        XmlNode node = doc.DocumentElement.SelectSingleNode(query);
        return float.Parse(node.InnerText);
    }

    private string FetchUVXML()
    {
        HttpWebRequest request = (HttpWebRequest) WebRequest.Create("https://uvdata.arpansa.gov.au/xml/uvvalues.xml");
        using (var response = (HttpWebResponse) request.GetResponse())
        {
            var encoding = Encoding.GetEncoding(response.CharacterSet);

            using (var responseStream = response.GetResponseStream())
            using (var reader = new StreamReader(responseStream, encoding))
                return reader.ReadToEnd();
        }
    }

    private XmlDocument GetUVXmlDocument(string contents)
    {
        XmlDocument doc = new XmlDocument();
        doc.LoadXml(contents);
        return doc;
    }
}


Action<IConfigContext> doConfig = (context) =>
{
    // Uncomment to switch update branch (or to disable updates)
    //context.Branch = Branch.None;

    SD.Color bgC = SD.ColorTranslator.FromHtml("#073642");
    SD.Color fgC = SD.ColorTranslator.FromHtml("#9e9090");
    Color backgroundColor = new Color(bgC.R, bgC.G, bgC.B);
    Color foregroundColor = new Color(fgC.R, fgC.G, fgC.B);
    context.AddBar( new BarPluginConfig() {
        BarTitle = "workspacer.Bar",
        Background = backgroundColor,
        DefaultWidgetBackground = backgroundColor,
        DefaultWidgetForeground = foregroundColor,
        BarHeight = 18,
        FontSize = 10,
        FontName = "IBM Plex Mono",

        RightWidgets = () => new IBarWidget[] {
            new WeatherWidget("Brisbane", "YBBN", 1000),
            new UVWidget("bri", 1000),
            new TimeWidget(1000, "| ddd dd.MMM.yyyy HH:mm ")
            },
    });
    context.AddFocusIndicator();

    var actionMenu = context.AddActionMenu();
    actionMenu.DefaultMenu.Add("Explorer", () => Process.Start("explorer"));
    actionMenu.DefaultMenu.Add("Edge", () => Process.Start("http://"));

    context.WorkspaceContainer.CreateWorkspaces("1", "2", "3", "4", "5");
    // context.CanMinimizeWindows = true; // false by default

    context.Keybinds.Subscribe(KeyModifiers.Alt, Keys.Tab, () => context.Workspaces.FocusedWorkspace.FocusNextWindow());
};
return doConfig;
