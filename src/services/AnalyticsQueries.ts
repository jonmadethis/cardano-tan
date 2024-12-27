import { QueryBuilder } from './QueryBuilder';

export class AnalyticsQueries {
    private queryBuilder: QueryBuilder;

    constructor(queryBuilder: QueryBuilder) {
        this.queryBuilder = queryBuilder;
    }

    getPlayerEngagementQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'player_address',
                'COUNT(DISTINCT game_id) as games_played',
                'SUM(bet_amount) as total_wagered',
                'AVG(bet_amount) as average_bet',
                'COUNT(CASE WHEN won = true THEN 1 END) as wins',
                'MAX(timestamp) as last_played'
            ])
            .from('game_events')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .groupBy('player_address')
            .toString();
    }

    getTransactionMetricsQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'COUNT(*) as total_transactions',
                'COUNT(CASE WHEN status = \'success\' THEN 1 END) as successful_transactions',
                'AVG(confirmation_time) as avg_confirmation_time',
                'AVG(gas_used) as avg_gas_used',
                'COUNT(DISTINCT player_address) as unique_players'
            ])
            .from('transactions')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .toString();
    }

    getGameDurationQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'game_id',
                'MAX(timestamp) - MIN(timestamp) as duration',
                'COUNT(DISTINCT player_address) as player_count',
                'SUM(bet_amount) as total_pot'
            ])
            .from('game_events')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .groupBy('game_id')
            .toString();
    }

    getBettingPatternsQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'chosen_number',
                'COUNT(*) as times_chosen',
                'AVG(bet_amount) as avg_bet_size',
                'EXTRACT(HOUR FROM timestamp) as hour_of_day'
            ])
            .from('bets')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .groupBy('chosen_number, EXTRACT(HOUR FROM timestamp)')
            .orderBy('hour_of_day, chosen_number')
            .toString();
    }

    getPerformanceMetricsQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'AVG(block_time - submission_time) as avg_block_time',
                'MAX(concurrent_players) as peak_concurrent_players',
                'COUNT(CASE WHEN status = \'timeout\' THEN 1 END) as timeout_count',
                'AVG(memory_usage) as avg_memory_usage',
                'MAX(memory_usage) as peak_memory_usage'
            ])
            .from('performance_metrics')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .toString();
    }

    getPlayerRetentionQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'player_address',
                'COUNT(DISTINCT DATE_TRUNC(\'day\', timestamp)) as days_active',
                'MAX(timestamp) - MIN(timestamp) as player_lifespan',
                'AVG(EXTRACT(EPOCH FROM (LEAD(timestamp) OVER (PARTITION BY player_address ORDER BY timestamp) - timestamp))) as avg_time_between_games'
            ])
            .from('game_events')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .groupBy('player_address')
            .having('COUNT(DISTINCT DATE_TRUNC(\'day\', timestamp)) > 1')
            .toString();
    }

    getRevenueAnalysisQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'DATE_TRUNC(\'day\', timestamp) as date',
                'SUM(house_fee) as total_fees',
                'COUNT(DISTINCT game_id) as games_played',
                'AVG(total_pot) as average_pot_size',
                'SUM(CASE WHEN total_pot > 1000000000 THEN 1 END) as high_value_games'
            ])
            .from('game_completions')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .groupBy('DATE_TRUNC(\'day\', timestamp)')
            .orderBy('date')
            .toString();
    }

    getErrorAnalysisQuery(timeframe: string): string {
        return this.queryBuilder
            .select([
                'error_type',
                'COUNT(*) as occurrence_count',
                'AVG(resolution_time) as avg_resolution_time',
                'MAX(timestamp) as last_occurrence',
                'COUNT(DISTINCT player_address) as affected_players'
            ])
            .from('error_events')
            .where(`timestamp >= NOW() - INTERVAL '${timeframe}'`)
            .groupBy('error_type')
            .orderBy('occurrence_count DESC')
            .toString();
    }
}
