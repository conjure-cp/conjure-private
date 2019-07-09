import * as React from "react"
import * as ReactDOM from "react-dom"
import Node from "../modules/Node"
import TreeVis from "./TreeVis"
import StatsBar from "./StatsBar"
import { HotKeys, GlobalHotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max } from "lodash"
import * as d3 from "d3"
import { Domains } from "./Domains"
import SplitPane from "react-split-pane"
import { Wrapper } from "./Constants"
import { Check } from "./Check"
import * as TreeHelper from "../modules/TreeHelper"
import * as MovementHelper from "../modules/MovementHelper"

interface FromServerNode {
  id: number
  parentId: number
  label: string
  prettyLabel: string
  childCount: number
  isSolution: boolean
  isLeftChild: boolean
  descCount: number
}

export type MyMap = Record<number, Node>

export interface Core {
  nodes: FromServerNode[]
  solAncestorIds: number[]
  id: string
}

interface Props {
  identifier: string
  core: Core
  info: string
  path: string
  nimServerPort: number
  playing: boolean
  reverse: boolean
  loadDepth: number
  duration: number
  finishedPlayingHandler: () => void
}

export interface State {
  id2Node: MyMap
  solveable: boolean
  selected: number
  linScale: any
  minsize: number
  shouldGetKids: boolean
  solNodeIds: number[]
  totalNodeCount: number
}

export class TreeContainer extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  map = {
    goLeft: ["left", "s", "a", "down"],
    goRight: ["right", "d"],
    goUp: ["up", "w"],
    collapse: "c",
    expand: "e",
    pPressed: "p",
    goToRoot: "r",
    goPrev: "shift"
  }

  handlers: any

  constructor(props: Props) {
    super(props)
    this.state = TreeHelper.makeState(this.props.core)

    this.handlers = {
      goLeft: () => MovementHelper.goLeft(this),
      goUp: () => MovementHelper.goUp(this),
      goRight: () => MovementHelper.goRight(this),
      goToRoot: () => this.setState({ selected: 0 }),
      goPrev: () => MovementHelper.goToPreviousHandler(this),
      collapse: this.collapse,
      expand: this.expand
    }
  }

  nodeClickHandler = (d: Node) => {
    this.setState({ selected: d.id })
  }

  collapse = () => {
    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node)
      Node.collapseNode(newMap[prevState.selected])
      return { id2Node: newMap }
    })
  }

  expand = () => {
    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node)
      Node.expandNode(newMap[prevState.selected])
      return { id2Node: newMap }
    })
  }

  play = async () => {
    while (this.props.playing) {
      if (
        (this.state.selected === last(this.props.core.solAncestorIds)! &&
          !this.props.reverse) ||
        (this.state.selected === 0 && this.props.reverse)
      ) {
        break
      }
      if (this.props.reverse) {
        MovementHelper.goToPreviousHandler(this)
      } else {
        MovementHelper.goLeft(this)
      }
      await TreeHelper.sleep(this.props.duration)
    }
    this.props.finishedPlayingHandler()
  }

  componentDidUpdate = (prevProps: Props) => {
    // Typical usage (don't forget to compare props):
    if (this.props.core.id !== prevProps.core.id) {
      this.setState(TreeHelper.makeState(this.props.core))
    }

    if (this.props.playing !== prevProps.playing) {
      this.play()
    }
  }

  render = () => {
    let failedBranchCount =
      this.state.totalNodeCount -
      (this.state.solveable ? this.props.core.solAncestorIds.length : 0)

    return (
      <div className="treeContainer">
        <HotKeys keyMap={this.map} handlers={this.handlers}>
          <StatsBar
            info={this.props.info}
            nextSolBranchHandler={() => MovementHelper.nextSolBranch(this)}
            prevSolBranchHandler={() => MovementHelper.prevSolBranch(this)}
            nextNodeHandler={() => MovementHelper.goLeft(this)}
            prevNodeHandler={() => MovementHelper.goToPreviousHandler(this)}
            nextFailedHandler={() => MovementHelper.nextFailed(this)}
            prevFailedHandler={() => MovementHelper.prevFailed(this)}
            nextSolHandler={() => MovementHelper.nextSol(this)}
            prevSolHandler={() => MovementHelper.prevSol(this)}
            minsize={this.state.minsize}
            solNodeIds={this.state.solNodeIds}
            totalNodes={this.state.totalNodeCount}
            failedBranchCount={failedBranchCount}
            linScale={this.state.linScale}
          />

          <Wrapper>
            <SplitPane split="horizontal" defaultSize={600}>
              <TreeVis
                id={this.props.core.id}
                identifier={this.props.identifier}
                rootNode={this.state.id2Node[0]}
                selected={this.state.selected}
                solAncestorIds={this.props.core.solAncestorIds}
                solveable={this.state.solveable}
                linScale={this.state.linScale}
                minsize={this.state.minsize}
                nodeClickHandler={this.nodeClickHandler}
                duration={this.props.duration}
                width={1200}
                height={500}
              />

              <Domains
                id={this.props.core.id}
                selected={this.state.selected}
                path={this.props.path}
                nimServerPort={this.props.nimServerPort}
              />
            </SplitPane>
          </Wrapper>
        </HotKeys>
      </div>
    )
  }
}
