# CEA Shiny App - Interactive Cost-Effectiveness Analysis Tool

**Interactive web application for cost-effectiveness analysis**

## Technology Stack

- **Backend**: R + dampack + ggplot2/plotly
- **Frontend**: Shiny 
- **Analysis**: Built on proven CEA packages (dampack, potentially hesim)

## Development Roadmap

### Phase 1: Core Functionality: done
- Manual strategy input interface
- Basic ICER calculations with dampack
- Cost-effectiveness plane and ICER table outputs
- Parameter input forms (outcome types, thresholds)

### Phase 2: Advanced Analysis (Weeks 4-6)
- Probabilistic sensitivity analysis integration
- Tornado diagrams for one-way sensitivity analysis
- CEAC curve generation
- CSV upload functionality

### Phase 3: Polish & Deployment (Weeks 7-8)
- Interactive plot enhancements (plotly integration)
- Export capabilities (PDF reports, Excel tables)
- Input validation and error handling
- Documentation and deployment

### Phase 4: Data Storage & Management (Future)
- Database integration to store analysis results
- Intervention categorization system (broad categories + specific interventions)
